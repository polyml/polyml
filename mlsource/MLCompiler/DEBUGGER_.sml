(*
    Title:      Source level debugger for Poly/ML
    Author:     David Matthews
    Copyright  (c)   David Matthews 2000, 2014

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
    |   EnvStaticLevel
    |   EnvStructure of string * signatures * locationProp list
    |   EnvTConstr of string * typeConstrSet

    datatype debugReason =
        DebugEnter of machineWord * types
    |   DebugLeave of machineWord * types
    |   DebugException of exn
    |   DebugStep

    (* We pass an integer code plus a value as arguments to the debugger function
       rather than a datatype because it's simpler when passing arguments through
       the bootstrap. *)
    type debugger = int * values * int * string * string * nameSpace -> unit
    (* Create a tag so that the debugger can be included in the parameters. *)
    val debuggerFunTag : debugger Universal.tag = Universal.tag()
    fun nullDebug _ = ()

    (* When stopped at a break-point any Bound ids must be replaced by Free ids.
       We make new Free ids at this point.  *)
    fun envTypeId (id as TypeId{ description, idKind = Bound _, ...}) =
            EnvTypeid { original = id, freeId = makeFreeId(Global CodeZero, isEquality id, description) }
    |   envTypeId id = EnvTypeid { original = id, freeId = id }

    (* Reason codes passed to the debugger function. *)
    val debugEnterFun = 1
    and debugLeaveFun = 2
    and debugExceptFun = 3
    and debugLineChange = 4
    
    val dummyValue = mkGvar("", TYPETREE.unitType, CodeZero, [])


    fun searchEnvs match (staticEntry :: statics, dlist as dynamicEntry :: dynamics) =
    (
        case (match (staticEntry, dynamicEntry), staticEntry) of
            (SOME result, _) => SOME result
        |   (NONE, EnvTypeid _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvVConstr _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvValue _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvException _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvStructure _) => searchEnvs match (statics, dynamics)
                (* EnvStaticLevel and EnvTConstr don't have entries in the dynamic list *)
        |   (NONE, EnvStaticLevel) => searchEnvs match (statics, dlist)
        |   (NONE, EnvTConstr _) => searchEnvs match (statics, dlist)
            
    )
    |   searchEnvs _ ([], []) = NONE
    
    |   searchEnvs _ _ = raise Misc.InternalError "searchEnvs: Static and dynamic lists have different lengths"

    fun makeSpace debugEnviron =
    let
        (* Values must be copied so that compile-time type IDs are replaced by their run-time values. *)
        local
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

            fun copyId(TypeId{idKind=Free _, access=Global _ , ...}) = NONE (* Use original *)
            |   copyId id = SOME(searchType debugEnviron id)
        in
            fun runTimeType ty =
                copyType (ty, fn x => x,
                    fn tcon => copyTypeConstr (tcon, copyId, fn x => x, fn s => s))

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
            fun replaceSignature (Signatures{ name, tab, typeIdMap, minTypes, maxTypes, declaredAt, ... }) =
            let
                fun getFreeId n = searchType debugEnviron (makeBoundId(Global CodeZero, n, false, false, basisDescription ""))
            in
                makeSignature(name, tab, minTypes, maxTypes, declaredAt, composeMaps(typeIdMap, getFreeId), [])
            end
        end

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

        |  lookupValues (EnvStaticLevel :: ntl, vl) s =
                (* Static level markers have no effect here. *)
                lookupValues(ntl, vl) s

        |  lookupValues (EnvTypeid _ :: ntl, _ :: vl) s = lookupValues(ntl, vl) s

        |  lookupValues (EnvStructure _ :: ntl, _ :: vl) s = lookupValues(ntl, vl) s

        |  lookupValues (EnvTConstr _ :: ntl, vl) s = lookupValues(ntl, vl) s

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

         |  allValues (EnvStaticLevel :: ntl, vl) = allValues(ntl, vl)

         |  allValues (EnvTypeid _ :: ntl, _ :: vl) = allValues(ntl, vl)

         |  allValues (EnvStructure _ :: ntl, _ :: vl) = allValues(ntl, vl)

         |  allValues (EnvTConstr _ :: ntl, vl) = allValues(ntl, vl)

         |  allValues _ = []

        fun lookupTypes (EnvValue _ :: ntl, _ :: vl) s = lookupTypes(ntl, vl) s

        |   lookupTypes (EnvException _ :: ntl, _ :: vl) s = lookupTypes(ntl, vl) s

        |   lookupTypes (EnvVConstr _ :: ntl, _ :: vl) s = lookupTypes(ntl, vl) s

        |   lookupTypes (EnvStaticLevel :: ntl, vl) s = lookupTypes(ntl, vl) s

        |   lookupTypes (EnvTypeid _ :: ntl, _ :: vl) s = lookupTypes(ntl, vl) s

        |   lookupTypes (EnvStructure _ :: ntl, _ :: vl) s = lookupTypes(ntl, vl) s

        |   lookupTypes (EnvTConstr (name, tcSet) :: ntl, vl) s =
                if name = s
                then SOME (copyTheTypeConstructor tcSet)
                else lookupTypes(ntl, vl) s

        |   lookupTypes _ _ = NONE

        fun allTypes (EnvValue _ :: ntl, _ :: vl) = allTypes(ntl, vl)

         |  allTypes (EnvException _ :: ntl, _ :: vl) = allTypes(ntl, vl)

         |  allTypes (EnvVConstr _ :: ntl, _ :: vl) = allTypes(ntl, vl)

         |  allTypes (EnvStaticLevel :: ntl, vl) = allTypes(ntl, vl)

         |  allTypes (EnvTypeid _ :: ntl, _ :: vl) = allTypes(ntl, vl)

         |  allTypes (EnvStructure _ :: ntl, _ :: vl) = allTypes(ntl, vl)

         |  allTypes (EnvTConstr(name, tcSet) :: ntl, vl) =
                (name, copyTheTypeConstructor tcSet) :: allTypes(ntl, vl)

         |  allTypes _ = []

        fun lookupStructs (EnvValue _ :: ntl, _ :: vl) s = lookupStructs(ntl, vl) s

        |   lookupStructs (EnvException _ :: ntl, _ :: vl) s = lookupStructs(ntl, vl) s

        |   lookupStructs (EnvVConstr _ :: ntl, _ :: vl) s = lookupStructs(ntl, vl) s

        |   lookupStructs (EnvStaticLevel :: ntl, vl) s = lookupStructs(ntl, vl) s

        |   lookupStructs (EnvTypeid _ :: ntl, _ :: vl) s = lookupStructs(ntl, vl) s

        |   lookupStructs (EnvStructure (name, rSig, locations) :: ntl, valu :: vl) s =
                if name = s
                then SOME(makeGlobalStruct (name, replaceSignature rSig, mkConst valu, locations))
                else lookupStructs(ntl, vl) s

        |   lookupStructs (EnvTConstr _ :: ntl, vl) s = lookupStructs(ntl, vl) s

        |   lookupStructs _ _ = NONE

        fun allStructs (EnvValue _ :: ntl, _ :: vl) = allStructs(ntl, vl)

         |  allStructs (EnvException _ :: ntl, _ :: vl) = allStructs(ntl, vl)

         |  allStructs (EnvVConstr _ :: ntl, _ :: vl) = allStructs(ntl, vl)

         |  allStructs (EnvStaticLevel :: ntl, vl) = allStructs(ntl, vl)

         |  allStructs (EnvTypeid _ :: ntl, _ :: vl) = allStructs(ntl, vl)

         |  allStructs (EnvStructure (name, rSig, locations) :: ntl, valu :: vl) =
                (name, makeGlobalStruct(name, replaceSignature rSig, mkConst valu, locations)) :: allStructs(ntl, vl)

         |  allStructs (EnvTConstr _ :: ntl, vl) = allStructs(ntl, vl)

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
    end;

    (* A pointer to this function is inserted in the code for each line. *)
    (* Although the nameTypeList and valueList are the same
       length we build them separately.  This allows the
       nameTypeList to be built at compile time and reduces
       the run-time costs. *)
    fun debugFunction (debugger, reason, functionName, location) staticEnv valueList =
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

        val (code, value) =
            case reason of
                DebugEnter (argValue, argType) =>
                    (debugEnterFun, mkGvar("", argType, mkConst argValue, [DeclaredAt location]))
            |   DebugLeave (fnResult, resType) =>
                    (debugLeaveFun, mkGvar("", resType, mkConst fnResult, [DeclaredAt location]))
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
        debugger(code, value, #startLine location, #file location, processedName, makeSpace(staticEnv, valueList))
    end

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
            fun loadStruct (str, (decs, (ctEnv, rtEnv))) =
                let
                    val loadStruct = codeStruct (str, level)
                    val newEnv = mkTuple [ loadStruct (* Structure. *), rtEnv level ]
                    val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                    val ctEntry =
                        case str of
                            NoStruct => raise Misc.InternalError "loadStruct: NoStruct"
                        |   Struct { name, signat, locations, ...} =>
                                EnvStructure(name, signat, locations)
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
    end
end;
