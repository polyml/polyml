(*
    Copyright (c) 2009 David C. J. Matthews

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

functor TYPEIDCODE (
    structure LEX : LEXSIG;
    structure CODETREE : CODETREESIG
    structure TYPETREE : TYPETREESIG

    structure STRUCTVALS : STRUCTVALSIG;

    structure DEBUG :
    sig
        val printDepthFunTag : (unit->int) Universal.tag
        val errorDepthTag: int Universal.tag
        val getParameter :
           'a Universal.tag -> Universal.universal list -> 'a
    end;

    structure PRETTY : PRETTYSIG;

    structure ADDRESS :
    sig
        type machineWord;
        type address;   (* an object that's represented as a pointer *) 
        type short = Word.word;   (* an object that's represented as a 30-bit int *)
        val toMachineWord : 'a    -> machineWord;
        val toAddress : 'a -> address;
        val F_words     : Word8.word;
        val F_bytes     : Word8.word;
        val F_mutable   : Word8.word;
        val isShort   : 'a -> bool;
        val loadWord:   (address * short) -> machineWord;
    end;
    
    sharing LEX.Sharing = STRUCTVALS.Sharing = PRETTY.Sharing = CODETREE.Sharing
            = TYPETREE.Sharing = ADDRESS
) : TYPEIDCODESIG =
struct
    open CODETREE PRETTY ADDRESS STRUCTVALS TYPETREE
    open RuntimeCalls
    
    (* This module deals with handling the run-time values that carry type
       information.  At the moment that's just the equality and print
       operations but that will be extended.
       
       There are different versions according to whether this is a
       monomorphic constructor, a polymorphic constructor or a type.
       Monomorphic and polymorphic constructor values are passed around
       in the module system as run-time values for types and datatypes
       whereas type values are passed in the core language as an extra
       argument to polymorphic functions.

       Both monomorphic and polymorphic constructors contain a reference
       for the "printer" entry so that a pretty printer can be installed.
       The functions in polymorphic datatypes have to be applied to type
       values for the base types to construct a type value.  Monomorphic
       datatypes just need some transformation.
       The effective types in each case are
       PolyType : (T('a) -> <'a t, 'a t> -> bool) * (T('a) -> 'a t * int -> pretty) ref
       MonoType : (<t * t> -> bool) * (t * int -> pretty) ref
       Type:      (<t * t> -> bool) * (t * int -> pretty)
       where < > denotes multiple (poly-style) arguments rather than tuples.
       *)
    structure TypeConstructorValue =
    struct
        val equalityOffset = 0
        and printerOffset = 1

        fun extractEquality idCode = mkInd(equalityOffset, idCode)
        and extractPrinterRef idCode = mkInd(printerOffset, idCode)
        
        fun createTypeValue{eqCode, printRefCode} = mkTuple[eqCode, printRefCode]
    end

    and TypeValue =
    struct
        val equalityOffset = 0
        and printerOffset = 1

        fun extractEquality idCode = mkInd(equalityOffset, idCode)
        and extractPrinter idCode = mkInd(printerOffset, idCode)
        
        fun createTypeValue{eqCode, printCode} = mkTuple[eqCode, printCode]
    end;

    val arg1     = mkLoad (~1, 0) (* Used frequently. *)
    val arg2     = mkLoad (~2, 0)

    val InternalError = Misc.InternalError

    val orb = Word8.orb
    infix 7 orb;
    val mutableFlags = F_words orb F_mutable;

    (* codeStruct and codeAccess are copied from ValueOps. *)
    fun codeStruct (str, level) =
        if isUndefinedStruct str
        then CodeNil
        else codeAccess (structAccess str, level)

    and codeAccess (Global code, _) = code
      
    |   codeAccess (Local{addr=ref locAddr, level=ref locLevel}, level) =
            mkLoad (locAddr, level - locLevel) (* No need for the recursive case. *)
     
    |   codeAccess (Selected{addr, base}, level) =
            mkInd (addr, codeStruct (base, level))
     
    |   codeAccess _ = raise InternalError "No access"

    (* Load an identifier. *)
    fun codeId(TypeId{access, ...}, level) = codeAccess(access, level)
    (* Pretty printer code.  These produce code to apply the pretty printer functions. *)
    fun codePrettyString(s: string) = mkConst(toMachineWord(PrettyString s))
    and codePrettyBreak(n, m) = mkConst(toMachineWord(PrettyBreak(n, m)))

    and codePrettyBlock(n: int, t: bool, c: context list, args: codetree) =
        mkEval(mkConst(toMachineWord(PrettyBlock)),
            [mkTuple[mkConst(toMachineWord n), mkConst(toMachineWord t),
             mkConst(toMachineWord c), args]],
            false)

    (* Turn a list of codetrees into a run-time list. *)
    and codeList(c: codetree list, tail: codetree): codetree =
        List.foldr (fn (hd, tl) => mkTuple[hd, tl]) tail c

    (* Generate code to check that the depth is not less than the allowedDepth
       and if it is to print "..." rather than the given code. *)
    and checkDepth(depthCode: codetree, allowedDepth: int, codeOk, codeFail) =
        mkIf(
            mkEval(
                rtsFunction POLY_SYS_int_lss,
                [depthCode, mkConst(toMachineWord allowedDepth)],
                true),
            codeFail,
            codeOk)

    structure TypeVarMap =
    struct
        (* Entries are either type var maps or "stoppers". *)
        datatype typeVarMapEntry =
            TypeVarFormEntry of (typeVarForm * int) list
        |   TypeConstrListEntry of typeConstrs list

        type typeVarMap =
        {
            entryType: typeVarMapEntry, (* Either the type var map or a "stopper". *)
            cache: (* Cache of new type values. *)
                {typeOf: types, address: int, decCode: codetree} list ref,
            mkAddr: int->int, (* Make new addresses at this level. *)
            level: int (* Function nesting level. *)
        } list

        (* Default map. *)
        fun defaultTypeVarMap (mkAddr, level) = [{entryType=TypeConstrListEntry[], cache=ref [], mkAddr=mkAddr, level=level}]

        fun markTypeConstructors(typConstrs, mkAddr, level, tvs) =
                {entryType = TypeConstrListEntry typConstrs, cache = ref [], mkAddr=mkAddr, level=level} :: tvs

        fun getCachedTypeValues(({cache=ref cached, ...}) ::_): codetree list =
                (* Extract the values from the list.  The later values may refer to earlier
                   so the list must be reversed. *)
                List.rev (List.map (fn{decCode, ...} => decCode) cached)
        |   getCachedTypeValues _ = raise Misc.InternalError "getCachedTypeValues"

        (* Extend a type variable environment with a new map of type variables to load functions. *)
        fun extendTypeVarMap (tvMap: (typeVarForm * int) list, mkAddr, level, typeVarMap) =
            {entryType = TypeVarFormEntry tvMap, cache = ref [], mkAddr=mkAddr, level=level} :: typeVarMap

        (* If we find the type var in the map return it as a type.  This is used to
           eliminate apparently generalisable type vars from the list. *)
        fun mapTypeVars [] _ = NONE

        |   mapTypeVars ({entryType=TypeVarFormEntry typeVarMap, ...} :: rest) tyVar =
            (
            case List.find(fn(t, _) => sameTv(t, tyVar)) typeVarMap of
                SOME (tv, _) => SOME(TypeVar tv)
            |   NONE => mapTypeVars rest tyVar
            )

        |   mapTypeVars (_ :: rest) tyVar = mapTypeVars rest tyVar

        (* Check to see if a type constructor is in the "stopper" set and return the level
           if it is. *)
        fun checkTypeConstructor(_, []) = ~1 (* Not there. *)
        |   checkTypeConstructor(tyCons, {entryType=TypeVarFormEntry _, ...} :: rest) =
                checkTypeConstructor(tyCons, rest: typeVarMap)
        |   checkTypeConstructor(tyCons, {entryType=TypeConstrListEntry tConstrs, ...} :: rest) =
                if List.exists(fn t => sameTypeId(tcIdentifier t, tcIdentifier tyCons)) tConstrs
                then List.length rest + 1
                else checkTypeConstructor(tyCons, rest)

        local
            (* The printer and equality functions must be valid functions even when they
               will never be called.  We may have to construct dummy type values
               by applying a polymorphic type constructor to them and if
               they don't have the right form the optimiser will complain. *)
            val errorFunction = mkProc(CodeZero, 0, 1, "errorCode")
            val codeFn = mkProc(codePrettyString "fn", 0, 1, "print-function")

            local
                fun typeValForMonotype typConstr =
                let
                    val codedId = codeId(tcIdentifier typConstr, 0)
                    val printerRefAddress = TypeConstructorValue.extractPrinterRef codedId
                    val printFn = (* Create a function to load the printer ref and apply to the args. *)
                        mkProc(
                            mkEval(
                                mkEval(rtsFunction POLY_SYS_load_word, [printerRefAddress, CodeZero], false),
                                [arg1], false),
                            0, 1, "print-" ^ tcName typConstr)
                    val eqFun = TypeConstructorValue.extractEquality codedId
                in
                    TypeValue.createTypeValue{eqCode=eqFun, printCode=printFn}
                end
                open STRUCTVALS (* Get the type constructors *)
            in
                (* A few common types.  These are effectively always cached. *)
                val intCode    = typeValForMonotype intType
                and boolCode   = typeValForMonotype boolType
                and stringCode = typeValForMonotype stringType
                and charCode   = typeValForMonotype charType
            end

            (* Code generate this now so we only get one entry. *)
            val codeTuple =
                mkTuple[
                    TypeValue.createTypeValue{eqCode=errorFunction, printCode=codeFn},
                    TypeValue.createTypeValue{eqCode=errorFunction, printCode=errorFunction},
                    intCode, boolCode, stringCode, charCode
                ]
            val code = genCode(codeTuple, [])()
            open STRUCTVALS
        in
            val functionCode = mkInd(0, code)
            (* Default code used for a type variable that is not referenced but
               needs to be provided to satisfy the type. *)
            val defaultTypeCode = mkInd(1, code)
            val cachedCode = [(intType, mkInd(2, code)), (boolType, mkInd(3, code)),
                              (stringType, mkInd(4, code)), (charType, mkInd(5, code))]
        end

        fun findCachedTypeCode(typeVarMap, typ): ((int->codetree) * int) option =
        let
            (* Test if we have the same type as the cached type. *)
            fun sameType (t1, t2) =
                case (eventual t1, eventual t2) of
                    (TypeVar tv1, TypeVar tv2) =>
                    (
                        case (tvValue tv1, tvValue tv2) of
                            (EmptyType, EmptyType) => sameTv(tv1, tv2)
                        |   _ => false
                    )
                |   (FunctionType{arg=arg1, result=result1}, FunctionType{arg=arg2, result=result2}) =>
                        sameType(arg1, arg2) andalso sameType(result1, result2)

                |   (LabelledType{recList=list1, ...}, LabelledType{recList=list2, ...}) =>
                        ListPair.allEq(
                            fn({name=n1, typeof=t1}, {name=n2, typeof=t2}) => n1 = n2 andalso sameType(t1, t2))
                            (list1, list2)
                
                |   (TypeConstruction{value=v1, args=a1, ...}, TypeConstruction{value=v2, args=a2, ...}) =>
                        sameTypeConstr(pling v1, pling v2) andalso ListPair.allEq sameType (a1, a2)

                |   _ => false

            and sameTypeConstr(tc1, tc2) = sameTypeId(tcIdentifier tc1, tcIdentifier tc2)


            fun findCodeFromCache([], _) = NONE
            |   findCodeFromCache({cache=ref cache, level, ...} :: rest, ty) =
                (
                    case List.find(fn {typeOf, ...} => sameType(typeOf, ty)) cache of
                        NONE => findCodeFromCache(rest, ty)
                    |   SOME{address, ...} => SOME(fn l => mkLoad(address, l-level), List.length rest +1)
                )
        in
            case typ of
                TypeVar tyVar =>
                (
                    case tvValue tyVar of
                        EmptyType =>
                        let (* If it's a type var it is either in the type var list or we return the
                               default.  It isn't in the cache. *)
                            fun findCodeFromTypeVar([], _) = ((fn _ => defaultTypeCode), 0)
                                (* Return default code for a missing type variable.  This can occur
                                   if we have unreferenced type variables that need to be supplied but
                                   are treated as "don't care". *)

                            |   findCodeFromTypeVar({entryType=TypeVarFormEntry typeVarMap, level, ...} :: rest, tyVar) =
                                (
                                case List.find(fn(t, _) => sameTv(t, tyVar)) typeVarMap of
                                    SOME(_, addr) => ((fn l => mkLoad(addr, l-level)), List.length rest+1)
                                |   NONE => findCodeFromTypeVar(rest, tyVar)
                                )

                            |   findCodeFromTypeVar(_ :: rest, tyVar) = findCodeFromTypeVar(rest, tyVar)
                        in
                            SOME(findCodeFromTypeVar(typeVarMap, tyVar))
                        end

                    |   OverloadSet _ =>
                        let
                            val constr = typeConstrFromOverload(typ, false)
                        in
                            findCachedTypeCode(typeVarMap, mkTypeConstruction(tcName constr, constr, [], []))
                        end

                    |   ty => findCachedTypeCode(typeVarMap, ty)
                )

            |   TypeConstruction { value, args, ...} =>
                    let
                        val typConstr = pling value
                        fun sameTypeConstr(tc1, tc2) = sameTypeId(tcIdentifier tc1, tcIdentifier tc2)
                    in
                        if tcIsAbbreviation typConstr (* Type abbreviation *)
                        then findCachedTypeCode(typeVarMap, makeEquivalent (typConstr, args))
                        else if null args
                        then (* Check the permanently cached monotypes. *)
                            case List.find(fn (t, _) => sameTypeConstr(t, typConstr)) cachedCode of
                                SOME (_, c) => SOME ((fn _ => c), ~1)
                            |   NONE => findCodeFromCache(typeVarMap, typ)
                        else findCodeFromCache(typeVarMap, typ)
                    end

            |   FunctionType _ => SOME(fn _ => functionCode, ~1) (* Every function has the same code. *)

            |   _ => findCodeFromCache(typeVarMap, typ)
        end

    end

    open TypeVarMap

    (* Find the earliest entry in the cache table where we can put this entry. *)
    fun getMaxDepth typeVarMap (ty: types, maxSoFar:int) : int =
        case findCachedTypeCode(typeVarMap, ty) of
            SOME (_, cacheDepth) => Int.max(cacheDepth, maxSoFar)
        |   NONE =>
            let
            in
                case ty of
                    TypeVar tyVar =>
                    (
                        case tvValue tyVar of
                            OverloadSet _ => maxSoFar (* Overloads are all global. *)
                        |   EmptyType => maxSoFar
                        |   tyVal => getMaxDepth typeVarMap (tyVal, maxSoFar)
                    )

                |   TypeConstruction{value, args, ...} =>
                    let
                        val constr = pling value
                    in
                        if tcIsAbbreviation constr  (* May be an alias *)
                        then getMaxDepth typeVarMap (makeEquivalent (constr, args), maxSoFar)
                        else List.foldl (getMaxDepth typeVarMap)
                                       (Int.max(maxSoFar, checkTypeConstructor(constr, typeVarMap))) args
                    end

                |   LabelledType {recList, ...} =>
                        List.foldl (fn ({typeof, ...}, m) =>
                                getMaxDepth typeVarMap (typeof, m)) maxSoFar recList

                |   _ => maxSoFar
            end

    (* Create a look-up function for type variables in a datatype or type constructor.
       This has to create a full type value even though only one field of it will be
       used but that maintains consistency with the rest of the typeVarMap. *)
    fun mkTcArgMap argTypes =
        let
            val nArgs = List.length argTypes
            val args = List.tabulate(nArgs, fn n => n-nArgs)
        in
            (ListPair.zipEq(argTypes, args), List.map (fn addr => mkLoad(addr, 0)) args)
        end

    fun printerForType(ty, baseLevel, argTypes: typeVarMap) =
    let
        fun printCode(typ, level) =
            case findCachedTypeCode(argTypes, typ) of
                SOME (code, _) => TypeValue.extractPrinter(code level)
            |   NONE =>
                (
                case typ of
                    typ as TypeVar tyVar =>
                    (
                        case tvValue tyVar of
                            EmptyType => raise InternalError "printerForType: should have been handled"

                        |   OverloadSet _ =>
                            let
                                val constr = typeConstrFromOverload(typ, false)
                            in
                                printCode(mkTypeConstruction(tcName constr, constr, [], []), level)
                            end

                        |   _ =>  (* Just a bound type variable. *) printCode(tvValue tyVar, level)
                    )

                |   TypeConstruction { value, args, ...} =>
                    let
                        val typConstr = pling value
                    in
                        if tcIsAbbreviation typConstr (* Handle type abbreviations directly *)
                        then printCode(makeEquivalent (typConstr, args), level)
                        else
                        let
                            (* Get the type Id and put in code to extract the printer ref. *)
                            val codedId = codeId(tcIdentifier typConstr, level)
                            val printerRefAddress = TypeConstructorValue.extractPrinterRef codedId
                            val printForConstructor =
                                mkEval(rtsFunction POLY_SYS_load_word, [printerRefAddress, CodeZero], false)
                        in
                            case args of
                                [] => printForConstructor
                            |   args =>
                                let
                                    (* Create printer functions for the args and apply the constructor
                                       function to them. *)
                                    val argList = map (fn t => TypeValue.createTypeValue{eqCode=CodeZero, printCode=printCode(t, level)}) args
                                in
                                    mkEval(printForConstructor, argList, true (* maybe early *))
                                end
                        end
                    end

                |   LabelledType { recList=[], ...} =>
                        (* Empty tuple: This is the unit value. *) mkProc(codePrettyString "()", 0, 1, "print-labelled")

                |   LabelledType { recList, frozen, ...} =>
                    let
                        (* See if this has fields numbered 1=, 2= etc. *)
                        fun isRec([], _) = true
                        |   isRec({name, ...} :: l, n) = name = Int.toString n andalso isRec(l, n+1)
                        val isTuple = frozen andalso isRec(recList, 1)
                        val localLevel = level+1
                        val valToPrint = mkInd(0, arg1) and depthCode = mkInd(1, arg1)

                        fun asRecord([], _, _) = raise Empty (* Shouldn't happen. *)

                        |   asRecord([{name, typeof, ...}], offset, _) =
                            let
                                val entryCode =
                                    (* Last or only field: no separator. *)
                                    if offset = 0
                                    then (* optimised unary records *)
                                        mkEval(printCode(typeof, localLevel), [arg1], false)
                                    else mkEval(printCode(typeof, localLevel),
                                                [mkTuple[mkInd(offset, valToPrint), depthCode]], false)
                                val (start, terminator) =
                                    if isTuple then ([], ")")
                                    else ([codePrettyString(name^" ="), codePrettyBreak(1, 0)], "}")
                            in
                                codeList(start @ [entryCode, codePrettyString terminator], CodeZero)
                            end

                        |   asRecord({name, typeof, ...} :: fields, offset, depth) =
                            let
                                val (start, terminator) =
                                    if isTuple then ([], ")")
                                    else ([codePrettyString(name^" ="), codePrettyBreak(1, 0)], "}")
                            in
                                checkDepth(depthCode, depth,
                                    codeList(
                                        start @
                                        [
                                            mkEval(
                                                printCode(typeof, localLevel),
                                                [mkTuple[mkInd(offset, valToPrint), depthCode]],
                                                false),
                                            codePrettyString ",",
                                            codePrettyBreak (1, 0)
                                        ],
                                        asRecord(fields, offset+1, depth+1)),
                                    codeList([codePrettyString ("..." ^ terminator)], CodeZero)
                                )
                            end
                    in
                        mkProc(
                            codePrettyBlock(1, false, [],
                                mkTuple[codePrettyString (if isTuple then "(" else "{"), asRecord(recList, 0, 0)]),
                            localLevel, 1, "print-labelled")
                    end

                |   FunctionType _ => raise InternalError "printerForType: should have been handled"
 
                |   _ => mkProc(codePrettyString "<empty>", 0, 1, "print-empty")
            )
    in
        printCode(ty, baseLevel)
    end

    and makeEq(ty, level, getEqFnForID, typeVarMap): codetree =
        case findCachedTypeCode(typeVarMap, ty) of
            SOME (code, _) => TypeValue.extractEquality(code level)
        |   NONE =>
            let
                fun equalityForConstruction(constr, args): codetree =
                (* Generate an equality function for a datatype construction. *)
                let
                    (* Get the equality functions for the argument types.
                       These want to be functions taking two arguments.
                       This applies only to polytypes. *)
                    fun getArg ty : codetree =
                    let
                        val eqFun = makeEq(ty, level, getEqFnForID, typeVarMap)
                    in
                        (* This now has to be a type value. *)
                        TypeValue.createTypeValue{eqCode=eqFun, printCode=CodeZero}
                    end

                    val resFun =
                    let
                        val iden = tcIdentifier constr
                    in
                        (* Special case: If this is ref, Array.array or Array2.array we must use
                           pointer equality and not attempt to create equality functions for
                           the argument.  It may not be an equality type. *)
                        if isPointerEqType iden
                        then rtsFunction POLY_SYS_word_eq
                        else
                        let
                            val (codeForId, directRecursion) = getEqFnForID(tcIdentifier constr, args, level)
                        in
                            case directRecursion of
                                SOME recCall => recCall (* It's a direct recusive call of the inner fn. *)
                            |   NONE => (* Apply the function we obtained to any type arguments. *)
                                    if null args
                                    then codeForId
                                    else mkEval(codeForId, map getArg args, true)
                        end
                    end
                in
                    resFun
                end
            in
                case ty of
                    TypeVar tyVar =>
                    (
                        case tvValue tyVar of
                            OverloadSet _ =>
                                 (* This seems to occur if there are what amount to indirect references to literals. *)
                                equalityForConstruction(typeConstrFromOverload(ty, false), [])

                        |   EmptyType => raise InternalError "makeEq: should already have been handled"

                        |   tyVal => makeEq(tyVal, level, getEqFnForID, typeVarMap)
                    )

                |   TypeConstruction{value, args, ...} =>
                    let
                        val constr = pling value
                    in
                        if tcIsAbbreviation constr  (* May be an alias *)
                        then makeEq (makeEquivalent (constr, args), level, getEqFnForID, typeVarMap)
                        else equalityForConstruction(constr, args)
                    end

                |   LabelledType {recList=[{typeof=singleton, ...}], ...} =>
                        (* Unary tuples are optimised - no indirection. *)
                        makeEq(singleton, level, getEqFnForID, typeVarMap)

                |   LabelledType {recList, ...} =>
                    (* Combine the entries.
                        fun eq(a,b) = #1 a = #1 b andalso #2 a = #2 b ... *)
                    let
                        (* Have to turn this into a new function. *)
                        val newLevel = level+1
                        fun combineEntries ([], _) = CodeTrue
                        |   combineEntries ({typeof, ...} :: t, n) =
                            let
                                val compareElements = makeEq(typeof, newLevel, getEqFnForID, typeVarMap)
                            in
                                mkCand(
                                    mkEval(compareElements, [mkInd(n, arg2), mkInd(n, arg1)], true),
                                    combineEntries (t, n+1))
                            end
                        val tupleCode = combineEntries(recList, 0)
                     in
                        mkProc(tupleCode, newLevel, 2, "eq{...}(2)")
                    end

                |   _ => raise InternalError "Equality for function"
            end

    (* Create equality functions for a set of possibly mutually recursive datatypes. *)
    fun equalityForDatatypes(typelist, eqAddresses, eqStatus, baseLevel, typeVarMap): codetree list =
    let
        val typesAndAddresses = ListPair.zipEq(typelist, eqAddresses)

        fun equalityForDatatype((tyConstr, addr), isEq) =
        if isEq
        then
        let
            val argTypes = tcTypeVars tyConstr

            (* Argument type variables. If it's a poly-type we need an extra function
               to apply to the type args so the depth is one more. *)
            val (localArgList, polyAdd, argTypeMap) =
                case argTypes of
                    [] => ([], 0, typeVarMap)
                |   _ =>
                    let
                        val (varToArgMap, localArgList) = mkTcArgMap argTypes
                        val addrs = ref 1 (* Make local declarations for any type values. *)
                        fun mkAddr n = !addrs before (addrs := !addrs + n)
                    in
                        (localArgList, 1, extendTypeVarMap(varToArgMap, mkAddr, baseLevel+1, typeVarMap))
                    end

            val nTypeVars = List.length argTypes

            (* If this is a reference to a datatype we're currently generating
               load that address otherwise fall back to the default. *)
            fun getEqFnForID(typeId, actualArgs, l) =
            let
                val code =
                    if sameTypeId(typeId, tcIdentifier tyConstr)
                    then mkLoad(0, l-baseLevel-1) (* Directly recursive. *)
                    else
                    case List.find(fn(tc, _) => sameTypeId(tcIdentifier tc, typeId)) typesAndAddresses of
                        SOME(_, addr) => mkLoad(addr, l-baseLevel) (* Mutually recursive. *)
                    |   NONE => TypeConstructorValue.extractEquality(codeId(typeId, l))
                (* If this is a recursive call and the type arguments that are being passed (if any) are
                   the same as the original arguments we can call the inner function directly.
                   e.g. if we have datatype 'a list = nil | :: of ('a * 'a list) the recursive
                   call has the same arguments but if we have datatype 'a t = X | Y of int t we
                   can't do this. *)
                val directRecursion =
                    if sameTypeId(typeId, tcIdentifier tyConstr) andalso
                            ListPair.foldlEq(fn (TypeVar tv, tv', true) => sameTv(tv, tv') | _ => false)
                                true (actualArgs, argTypes)
                    then SOME(mkLoad(0, l-baseLevel-(polyAdd+1)))
                    else NONE
            in
                (code, directRecursion)
            end

            (* Filter out the EnumForm constructors.  They arise
               in situations such as datatype t = A of int*int | B | C
               i.e. where we have only one non-nullary constructor
               and it is a tuple.  In this case we can deal with all
               the nullary constructors simply by testing whether
               the two arguments are the same.  We don't have to
               discriminate the individual cases. *)
            fun (*isEnum(Value{class=Constructor{nullary=true, ...}, access=Global code, ...}) =
            let
                open ADDRESS
            in
                (* If the value is a short integer then we can check
                   for equality using pointer equality. *)
                isShort(loadWord(toAddress(evalue code), 0w1))
            end
            | *)isEnum _ = false

            fun processConstrs [] =
                (* The last of the alternatives is false *) CodeZero

            |  processConstrs ((vConstr as Value{class, access, typeOf, ...}) ::rest) =
                if isEnum vConstr then processConstrs rest
                else
                let
                    fun addPolymorphism c = if nTypeVars = 0 then c else mkEval(c, localArgList, true)
                    val newLevel = baseLevel+polyAdd+1 (* We have one or two enclosing functions. *)
                    val base = codeAccess(access, newLevel)
                    fun matches arg =
                        mkEval(addPolymorphism(mkInd(0, base)) (* Test function. *), [arg], true)
                in
                    case class of
                        Constructor{nullary=true, ...} =>
                            mkIf(matches arg1, matches arg2, processConstrs rest)
                    |    _ => (* We have to unwrap the value. *)
                        let
                            (* Get the constructor argument given the result type.  We might
                               actually be able to take the argument type off directly but
                               there's some uncertainty about whether we use the same type
                               variables for the constructors as for the datatype. (This only
                               applies for polytypes). *)
                            val resType = constructorResult(typeOf, List.map TypeVar argTypes)

                            (* Code to extract the value. *)
                            fun destruct argNo =
                                mkEval(addPolymorphism(mkInd(2, codeAccess(access, newLevel))) (* projection function. *),
                                    [mkLoad(argNo, 0)], true)

                            (* Test whether the values match. *)
                            val eqValue =
                                mkEval(
                                    makeEq(resType, newLevel, getEqFnForID, argTypeMap),
                                    [destruct ~1, destruct ~2], true)
                        in
                            (* We have equality if both values match
                               this constructor and the values within
                               the constructor match. *)
                            mkIf(matches arg1, mkCand(matches arg2, eqValue), processConstrs rest)
                        end
                end

            (* We previously only tested for bit-wise (pointer) equality if we had
               at least one "enum" constructor in which case the test would eliminate
               all the enum constructors.  I've now extended this to all cases where
               there is more than one constructor.  The idea is to speed up equality
               between identical data structures. *)
            val vConstrs = tcConstructors tyConstr
            val eqCode =
                case vConstrs of
                   [vcons] => (* Single constructor. *)
                       if isEnum vcons
                       then CodeTrue (* Return true here: processConstrs would return false. *)
                       else processConstrs vConstrs
                 |  _ => (* More than one constructor: should never be zero. *)
                        mkCor(mkTestptreq(arg1, arg2), processConstrs vConstrs)
            val eqFun =
                mkProc(eqCode, baseLevel+polyAdd, 2, "eq-" ^ tcName tyConstr ^ "(2)")
            val code =
                if polyAdd = 0
                then eqFun
                else mkProc(mkEnv(getCachedTypeValues argTypeMap @ [eqFun]), baseLevel, nTypeVars, "equality()")
        in
            mkDec(addr, code)
        end
        else (* Not an equality type.  This will not be called. *)
            mkDec(addr, CodeZero)
    in
        ListPair.map equalityForDatatype (typesAndAddresses, eqStatus)
    end

    (* Create a printer function for a datatype when the datatype is declared.
       We don't have to treat mutually recursive datatypes specially because
       this is called after the type IDs have been created. *)
    fun printerForDatatype(typeConstr, level, typeVarMap) =
    let
        val name = tcName typeConstr
        val argTypes = tcTypeVars typeConstr
        val argCode = mkInd(0, arg1)
        and depthCode = mkInd(1, arg1)

        val (localArgList, innerLevel, newTypeVarMap) =
            case argTypes of
                [] => ([], level+1, typeVarMap)
            |   _ =>
                let
                    val (varToArgMap, localArgList) = mkTcArgMap argTypes
                    val addrs = ref 1 (* Make local declarations for any type values. *)
                    fun mkAddr n = !addrs before (addrs := !addrs + n)
                in
                    (localArgList, level+2, extendTypeVarMap(varToArgMap, mkAddr, level+1, typeVarMap))
                end

        (* If we have an expression as the argument we parenthesise it unless it is
           a simple string, a tuple, a record or a list.
           A reference to this function is copied into the code.
           N.B.  This code is also in InitialBasis to handle "ref" and "option". *)
        fun parenthesise(s as PrettyBlock(_, _, _, [ _ ])) = s
        |   parenthesise(s as PrettyBlock(_, _, _, (PrettyString("(")::_ ))) = s
        |   parenthesise(s as PrettyBlock(_, _, _, (PrettyString("{")::_ ))) = s
        |   parenthesise(s as PrettyBlock(_, _, _, (PrettyString("[")::_ ))) = s
        |   parenthesise(s as PrettyBlock _) =
                PrettyBlock(3, true, [], [ PrettyString "(", PrettyBreak(0, 0), s, PrettyBreak(0, 0), PrettyString ")" ])
        |   parenthesise s = s (* String or Break *)

        fun printerForConstructors
                (Value{name, typeOf, access, class = Constructor{nullary, ...}, ...} :: rest, depth) =
            let
                (* The "value" for a value constructor is a tuple containing
                   the test code, the injection and the projection functions. *)
                val constructorCode = codeAccess(access, innerLevel)

                (* If this is a polytype the fields in the constructor tuple are functions that first
                   have to be applied to the type arguments to yield the actual injection/test/projection
                   functions.  For monotypes the fields contain the injection/test/projection
                   functions directly. *)
                fun addPolymorphism c =
                    if null argTypes then c else mkEval(c, localArgList, true)
                
                val printCode =
                    if nullary
                    then (* Just the name *) codePrettyString name
                    else
                    let
                        val typeOfArg =
                            case typeOf of
                                FunctionType{arg, ...} => arg
                            |   _ => raise InternalError "contructor not a function"
                        val getValue = mkEval(addPolymorphism(mkInd(2, constructorCode)), [argCode], true)
                        
                    in
                        codePrettyBlock(1, false, [],
                            codeList(
                                [
                                    codePrettyString name,
                                    codePrettyBreak (1, 0),
                                    (* Print the argument and parenthesise it if necessary. *)
                                    mkEval(mkConst(toMachineWord parenthesise),
                                        [
                                            mkEval(
                                                printerForType(typeOfArg, innerLevel, newTypeVarMap),
                                                [mkTuple[getValue, depthCode]],
                                                false
                                            )],
                                        false)
                                ], CodeZero))
                    end
            in
                (* If this was the last or only constructor we don't need to test. *)
                checkDepth(depthCode, depth,
                    if null rest
                    then printCode
                    else
                    let
                        val testValue = mkEval(addPolymorphism(mkInd(0, constructorCode)), [argCode], true)
                    in
                        mkIf(testValue, printCode, printerForConstructors(rest, depth+1))
                    end,
                    codePrettyString "...")
            end

        |   printerForConstructors _ = raise InternalError ("No constructors:"^name)
            
        val printerCode = printerForConstructors(tcConstructors typeConstr, 0)
    in
        (* Wrap this in the functions for the base types. *)
        if null argTypes
        then mkProc(printerCode, level+1, 1, "print-"^name)
        else mkProc(mkEnv(getCachedTypeValues newTypeVarMap @
                            [mkProc(printerCode, level+2, 1, "print-"^name)]),
                    level+1, List.length argTypes, "print"^name^"()")
    end    

    (* Opaque matching and functor application create new type IDs using an existing
       type as implementation.  The equality function is inherited whether the type
       was specified as an eqtype or not.  The print function is inherited but a new
       ref is created so that if a pretty printer is installed for the new type it
       does not affect the old type. *)
    (* If this is a type function we're going to generate a new ref anyway so we
       don't need to copy it. *)
    fun codeGenerativeId(sourceId as TypeId{typeFn=(_, EmptyType), ...}, _, _, level) =
        let
            open TypeConstructorValue
            (* TODO: Use multipleUses here. *)
            val sourceCode = codeId(sourceId, level)
        in
            createTypeValue {
                eqCode = extractEquality sourceCode,
                printRefCode =
                mkEval
                    (rtsFunction POLY_SYS_alloc_store,
                    [mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags),
                     mkEval(rtsFunction POLY_SYS_load_word,
                        [extractPrinterRef sourceCode, CodeZero], false)
                      ],
                false)  
            }
        end

    |   codeGenerativeId(TypeId{typeFn=(argTypes, resType), ...}, isEq, mkAddr, level) =
        let
            (* Create a new type value cache. *)
            val typeVarMap = defaultTypeVarMap(mkAddr, level)

            open TypeConstructorValue
            (* Create a printer for a type function.  This is used to create the general
               print function for any type. *)
            val printCode =
                case argTypes of
                    [] => printerForType(resType, level, typeVarMap)
                |   _ =>
                    let
                        val addrs = ref 1 (* Make local declarations for any type values. *)
                        fun mkAddr n = !addrs before (addrs := !addrs + n)
                        val argTypeMap = extendTypeVarMap(#1(mkTcArgMap argTypes), mkAddr, level+1, typeVarMap)
                        val printCode = printerForType(resType, level+1, argTypeMap)
                    in
                        (* Wrap this in a function for the type arguments. *)
                        mkProc(
                            mkEnv(getCachedTypeValues argTypeMap @ [printCode]),
                            level, List.length argTypes, "print-helper()")
                    end

            val eqCode =
                if not isEq then CodeZero
                else case argTypes of
                    [] =>
                    let
                        fun getEqFnForID(typeId, _, l) = (TypeValue.extractEquality(codeId(typeId, l)), NONE)
                    in
                        (* We need a function that takes two arguments rather than a single pair. *)
                        makeEq(resType, level, getEqFnForID, typeVarMap)
                    end
                |   _ =>
                    let
                        val addrs = ref 1 (* Make local declarations for any type values. *)
                        fun mkAddr n = !addrs before (addrs := !addrs + n)
                        val argTypeMap =
                            extendTypeVarMap(#1 (mkTcArgMap argTypes), mkAddr, level+1, typeVarMap)

                        fun getEqFnForID(typeId, _, l) = (TypeValue.extractEquality(codeId(typeId, l)), NONE)

                        val innerFnCode = makeEq(resType, level+1, getEqFnForID, argTypeMap)
                        (* We need a function that takes two arguments rather than a single pair. *)
                    in
                        mkInlproc(mkEnv(getCachedTypeValues argTypeMap @ [innerFnCode]), level, List.length argTypes, "equality()")
                    end
        in
            mkEnv(
                TypeVarMap.getCachedTypeValues typeVarMap @
                [createTypeValue {
                    eqCode = eqCode,
                    printRefCode =
                    mkEval
                        (rtsFunction POLY_SYS_alloc_store,
                        [mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags),
                         printCode],
                    false)  
                }])
        end


    (* Create the equality and type functions for a set of mutually recursive datatypes. *)
    fun createDatatypeFunctions(typelist, eqStatus, mkAddr, level, typeVarMap) =
    let
        (* Each entry has an equality function and a ref to a print function.
           The print functions for each type needs to indirect through the refs
           when printing other types so that if a pretty printer is later
           installed for one of the types the others will use the new pretty
           printer.  That means that the code has to be produced in stages. *)
        (* Create the equality functions.  Because mutual decs can only be functions we
           can't create the typeIDs themselves as mutual declarations. *)
        val eqAddresses = List.map(fn _ => mkAddr 1) typelist (* Make addresses for the equalities. *)
        val equalityFunctions =
            mkMutualDecs(equalityForDatatypes(typelist, eqAddresses, eqStatus, level, typeVarMap))

        (* Create the typeId values and set their addresses.  The print function is
           initially set as zero. *)
        local
            fun makeTypeId(tc, eqAddr) =
            let
                val var = vaLocal(idAccess(tcIdentifier tc))
                val newAddr = mkAddr 1
                val idCode =
                mkTuple
                [
                    mkLoad(eqAddr, 0),
                    mkEval
                        (rtsFunction POLY_SYS_alloc_store,
                        [mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags),
                         CodeZero (* Temporary - replaced by setPrinter. *)],
                    false)  
                ]
            in
                #addr var := newAddr;
                #level var:= level;
                mkDec(newAddr, idCode)
            end
        in
            val typeIdCode = ListPair.map makeTypeId (typelist, eqAddresses)
        end

        (* Create the print functions and set the printer code for each typeId. *)
        local
            fun setPrinter tc =
                mkEval(
                    rtsFunction POLY_SYS_assign_word,
                    [mkInd(1, codeId(tcIdentifier tc, level)),
                              CodeZero, printerForDatatype(tc, level, typeVarMap)],
                    false)
        in
            val printerCode = List.map setPrinter typelist
        end
    in
        equalityFunctions :: typeIdCode @ printerCode
    end


    (* Exported function.  Returns a function from an ML pair of values to bool.
       N.B. This differs from the functions in the typeID which take a Poly pair. *)
    fun equalityForType(ty: types, level: int, typeVarMap: typeVarMap): codetree =
    let
        fun getEqFnForID(typeId, _, l) = (mkInd(0, codeId(typeId, l)), NONE)
        (* The final result function must take a single argument. *)
        val resultCode = makeEq(ty, level+1, getEqFnForID, typeVarMap)
    in
        (* We need to wrap this up in a new inline function. *)
        mkInlproc(mkEval(resultCode, [mkInd(0, arg1), mkInd(1, arg1)], true),
                  level, 1, "equality")
    end

    (* This code is used when the type checker has to construct a unique monotype
       because a type variable has escaped to the top level.
       The equality code always returns true and the printer prints "?". *)
    fun codeForUniqueId() =
        mkConst(ADDRESS.toMachineWord((fn _ => true, ref(fn _ => fn _ => fn _ => PrettyString "?"))))

    (* If this is a polymorphic value apply it to the type instance. *)
    fun applyToInstance([], level, _, code) = code level (* Monomorphic. *)

    |   applyToInstance(sourceTypes, level, polyVarMap, code) =
    let
        (* See if we have this on the list and if so use it.  Otherwise create a new pair of
           equality and print functions. *)
        fun makePolyParameter t =
            case findCachedTypeCode(polyVarMap, t) of
                SOME (code, _) => code level
            |   NONE =>
                let
                    val maxCache = getMaxDepth polyVarMap (t, 1)
                    val cacheEntry = List.nth(polyVarMap, List.length polyVarMap - maxCache)
                    val { cache, mkAddr, level=decLevel, ...} = cacheEntry
                    local
                        val eqCode =
                            if typePermitsEquality t
                            then
                            let
                                fun getEqFnForID(typeId, _, l) = (mkInd(0, codeId(typeId, l)), NONE)
                            in
                                (* Unlike equalityForType we want a pair argument here not a tuple. *)
                                makeEq(t, decLevel, getEqFnForID, polyVarMap)
                            end
                            else mkProc(CodeZero, 0, 1, "errorCode")
                    in
                        val typeValue = mkTuple[eqCode, printerForType(t, decLevel, polyVarMap)]
                    end
                    (* Make a new entry and put it in the cache. *)
                    val decAddr = mkAddr 1
                    val () = cache := {decCode = mkDec(decAddr, typeValue), typeOf = t, address = decAddr } :: !cache
                in
                    mkLoad(decAddr, level-decLevel)
                end
    in
        mkEval(code level, List.map makePolyParameter sourceTypes, true)
    end

    structure Sharing =
    struct
        type typeId     = typeId
        type codetree   = codetree
        type types      = types
        type typeConstrs= typeConstrs
        type typeVarForm=typeVarForm
        type typeVarMap = typeVarMap
    end
end;
