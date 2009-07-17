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
    val arg1     = mkLoad (~1, 0) (* Used frequently. *)
    val arg2     = mkLoad (~2, 0)

    val InternalError = Misc.InternalError

    val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;

    val andb = Word8.andb and orb = Word8.orb
    infix 6 andb;
    infix 7 orb;
    val mutableFlags = F_words orb F_mutable;

    (* Pretty printer code.  These produce code to apply the pretty printer functions. *)
    fun codePrettyString(s: string) = mkConst(toMachineWord(PrettyString s))
    and codePrettyStringVar(s: codetree) =
        mkEval(mkConst(toMachineWord(PrettyString)), [s], false)
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
                mkConst(toMachineWord(ioOp POLY_SYS_int_lss)),
                [depthCode, mkConst(toMachineWord allowedDepth)],
                true),
            codeFail,
            codeOk)

    (* Subtract a depth from the current depth to produce the depth for sub-elements. *)
    and adjustDepth(depthCode, 0) = depthCode
    |   adjustDepth(depthCode, n) =
        mkEval(mkConst(toMachineWord(ioOp POLY_SYS_aminus)),
               [depthCode, mkConst(toMachineWord n)],
               true)

    (* Equality code needs to be as efficient as possible so we take care
       about code size. To reduce the size of the code we pass down the kind of
       result we want. *)
    datatype reskind =
        ApplyFun of (int->codetree)*(int->codetree)
    |   MakeFun
    (* If we get a function back it may take a pair as an argument or
       it may take two arguments. *)
    datatype resfun =
        PairArg of int -> codetree
    |   TwoArgs of int -> codetree

    (* If we have a function we either return it or we apply it.  The
       function will always take a single argument as a tuple. *)
    fun returnFun (f: resfun, MakeFun) = f
    |   returnFun (TwoArgs f, ApplyFun(a1, a2)) =
            PairArg(fn lA => mkEval(f lA, [a1 lA, a2 lA], true))
    |   returnFun (PairArg f, ApplyFun(a1, a2)) =
            PairArg(fn lA => mkEval(f lA, [mkTuple[a1 lA, a2 lA]], true))

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
    fun codeId(Free{access, ...}, level) = codeAccess(access, level)
    |   codeId(Bound{access, ...}, level) = codeAccess(access, level)
    |   codeId _ = raise InternalError "codeId: Unknown form"

    (* Create a printer for a type function.  This is used to create the general
       print function for any type. *)
    and printerForTypeFunction(argTypes, resType, level) =
        (* Wrap this in the functions for the depth and the type arguments. *)
        mkProc(
            mkProc(
                mkProc(
                    printCodeForType(
                        resType, mkLoad(~1, 0), 0, mkLoad(~1, 2), level+3, argTypes),
                    level+2, 1, "print-helper"),
                level+1, 1, "print-helper()"),
            level, 1, "print-helper()()")

    and printCodeForType(ty, valToPrint, depth, depthCode, baseLevel, argTypes) =
    let
        fun printCode(TypeVar tyVar, valToPrint, depth, depthCode, level) =
            if not (isEmpty(tvValue tyVar))
            then printCode(tvValue tyVar, valToPrint, depth, depthCode, level) (* Just a bound type variable. *)
            else (* It should be an argument. *)
            let
                fun findPos(_, []) = ~1 (* Not there. *)
                |   findPos(n, (hd::tl)) =
                        if sameTv(hd, tyVar)
                        then n
                        else findPos(n+1, tl)
                val position = findPos(0, argTypes)
            in
                if position < 0 (* Not there: must be in a polymorphic function. *)
                then codePrettyString "?"
                else (* Call the appropriate parameter function with a tuple containing
                        the value and the corrected depth. *)
                let
                    (* If this is a unary type the "argument" is the function itself,
                       otherwise it's a tuple. *)
                    val argCode =
                        case argTypes of
                            [_] => mkLoad(~1, level-baseLevel+1)
                    |   _ => mkInd(position, mkLoad(~1, level-baseLevel+1))
                in
                    mkEval(argCode, [mkTuple[valToPrint, adjustDepth(depthCode, depth)]], false)
                end
            end

        |   printCode(TypeConstruction { value, args, ...}, valToPrint, depth, depthCode, level) =
            let
                val typConstr = pling value
            in
                if tcIsAbbreviation typConstr (* Handle type abbreviations directly *)
                then printCode(makeEquivalent (typConstr, args),
                               valToPrint, depth, depthCode, level)
                else
                let
                    val codedId = codeId(tcIdentifier typConstr, level)
                        handle exn =>
                        (
                            print(concat["codedId: ", tcName typConstr, "\n"]);
                            raise exn
                        )
                    val printForConstructor =
                        mkEval(mkConst(ioOp POLY_SYS_load_word),
                            [mkInd(1, codedId), CodeZero], false)
                    (* The printer functions for the arguments have to
                       be tupled if there's more than one. *)
                    (* The argument printer functions have type 'a*int->pretty
                       i.e. they take a pair of the value and the depth. *)
                    fun makePrinterArgFunction t =
                        mkProc(
                            printCode(t, mkInd(0, mkLoad(~1, 0)), 0, mkInd(1, mkLoad(~1, 0)), level+1),
                            level, 1, concat["arg-printer:", tcName typConstr])
                
                    val argTuple =
                        case args of
                            []   => CodeZero
                          | [t]  => makePrinterArgFunction t
                          | args => mkTuple(map makePrinterArgFunction args)
                    (* Apply the function for the type constructor to the functions
                       for the argument types, then to the depth and finally to the
                       argument. *)
                in
                    mkEval(
                        mkEval(
                            mkEval(printForConstructor, [adjustDepth(depthCode, depth)], true (* This can be early *)),
                            [argTuple], true (* This can be early *)),
                        [valToPrint], false (* Not early *))
                end
            end

        |   printCode(LabelledType { recList=[], ...}, _, _, _, _) =
                (* Empty tuple: This is the unit value. *) codePrettyString "()"

        |   printCode(LabelledType { recList, frozen, ...}, valToPrint, depth, depthCode, level) =
            let
                (* See if this has fields numbered 1=, 2= etc. *)
                fun isRec([], _) = true
                |   isRec({name, typeof} :: l, n) = name = Int.toString n andalso isRec(l, n+1)
                val isTuple = frozen andalso isRec(recList, 1)

                fun asRecord([], _, _) = raise Empty (* Shouldn't happen. *)

                |   asRecord([{name, typeof, ...}], offset, depth) =
                    let
                        val entryCode =
                            (* Last or only field: no separator. *)
                            if offset = 0
                            then (* optimised unary records *)
                                printCode(typeof, valToPrint, depth, depthCode, level)
                            else printCode(typeof, mkInd(offset, valToPrint), depth, depthCode, level)
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
                                    printCode(typeof, mkInd(offset, valToPrint), depth, depthCode, level),
                                    codePrettyString ",",
                                    codePrettyBreak (1, 0)
                                ],
                                asRecord(fields, offset+1, depth+1)),
                            codeList([codePrettyString ("..." ^ terminator)], CodeZero)
                        )
                    end
            in
                codePrettyBlock(1, false, [],
                    mkTuple[codePrettyString (if isTuple then "(" else "{"), asRecord(recList, 0, depth+1)])
            end

        |   printCode(FunctionType _, _, _, _, _) = codePrettyString "fn"

        |   printCode _ = codePrettyString "<empty>"
    in
        printCode(ty, valToPrint, depth, depthCode, baseLevel)
    end

    and makeEq(ty: types, resKind: reskind,
               getEqFnForID: typeId * types list * int -> codetree * codetree option,
               findTyVars: typeVarForm -> resfun): resfun =
    let
        fun equalityForConstruction(constr, args, vConstrs): resfun =
        (* Generate an equality function for a datatype construction. *)
        let
            (* Get the equality functions for the argument types.
               These want to be functions taking two arguments.
               This applies only to polytypes. *)
            fun getArg level ty : codetree =
            let
                val eqFun = makeEq(ty, MakeFun, getEqFnForID, findTyVars)
            in
                case eqFun of
                    PairArg f =>
                            (* Have to make a function which takes two arguments. *)
                            mkInlproc(
                                mkEval(f(level+1), [mkTuple[arg1, arg2]], true),
                                level+1, 2, "eq-"^tcName constr^"(...)")
                |   TwoArgs f => f level
            end

            fun resFun l =
            let
                val iden = tcIdentifier constr
            in
                (* Special case: If this is ref, Array.array or Array2.array we must use
                   pointer equality and not attempt to create equality functions for
                   the argument.  It may not be an equality type. *)
                if isPointerEqType iden
                then mkConst(ioOp POLY_SYS_word_eq)
                else
                let
                    val (codeForId, directRecursion) = getEqFnForID(tcIdentifier constr, args, l)
                in
                    case directRecursion of
                        SOME recCall => recCall (* It's a direct recusive call of the inner fn. *)
                    |   NONE => (* Apply the function we obtained to any type arguments. *)
                            mkEval(codeForId, map (getArg l) args, true)
                end
            end
        in
            TwoArgs resFun
        end
    in
        case ty of
            TypeVar tyVar =>
            let
              (* The type variable may be bound to something. *)
              val tyVal = tvValue tyVar
            in
              (* If we have an unbound type variable it may either
                 be a type constructor argument or it may be a free
                 equality type variable. *)
              if isEmpty tyVal
              then returnFun(findTyVars tyVar, resKind)
              else makeEq(tyVal, resKind, getEqFnForID, findTyVars)
            end

        |   TypeConstruction{value, args, ...} =>
            let
                val constr = pling value
                val id = tcIdentifier constr
            in
                if tcIsAbbreviation constr  (* May be an alias *)
                then makeEq (makeEquivalent (constr, args), resKind, getEqFnForID, findTyVars)
                else returnFun(equalityForConstruction(constr, args, tcConstructors constr), resKind)
            end

        |   LabelledType {recList=[{typeof=singleton, ...}], ...} =>
                (* Unary tuples are optimised - no indirection. *)
                makeEq(singleton, resKind, getEqFnForID, findTyVars)

        |   LabelledType {recList, ...} =>
            (* Combine the entries.
                fun eq(a,b) = #1 a = #1 b andalso #2 a = #2 b ... *)
            let
                fun eqTuple(arg1, arg2, lA) =
                let
                    fun combineEntries ([], n) = CodeTrue
                    |    combineEntries ({typeof, name=_}::t, n) =
                            mkCand
                            (applyEq(typeof, fn l => mkInd(n, arg1 l),
                                     fn l => mkInd(n, arg2 l),
                                     lA, getEqFnForID, findTyVars),
                             combineEntries (t, n+1))
                in
                    combineEntries(recList, 0)
                end
            in
                case resKind of
                    ApplyFun(a1, a2) => PairArg(fn l => eqTuple(a1, a2, l))
                |   MakeFun =>
                    let
                        fun wrappedCode level =
                        let
                            val newLevel = level+1

                            val code = eqTuple(fn l => mkLoad(~1, l-newLevel),
                                               fn l => mkLoad(~2, l-newLevel),
                                               newLevel);
                        in
                            mkProc(code, newLevel, 2, "eq{...}(2)")
                        end
                    in
                        TwoArgs wrappedCode
                    end
            end

        |    _ => raise InternalError "Equality for function"
    end

    (* Make an equality function and apply it to the arguments. *)
    and applyEq(ty, arg1, arg2, lA, getEqFnForID, findTyVars): codetree =
    case makeEq(ty, ApplyFun(arg1, arg2), getEqFnForID, findTyVars) of
       PairArg c => c lA
    |  TwoArgs _ => raise InternalError "applyEq: wrong result"    

    (* Create an equality function for a type function. *)
    and equalityForTypeFunction(argTypes, resType, level) =
    let
        fun getEqFnForID(typeId, _, l) = (mkInd(0, codeId(typeId, l)), NONE)
        val nTypeVars = List.length argTypes
        fun typeVarFun tv =
        let
            fun findTv [] n = (* All type variables should be bound. *)
                    raise InternalError "Free type variable"
             |  findTv (tv' :: tvs) n =
                    if sameTv(tv, tv')
                    then TwoArgs(fn l => mkLoad(n, l-level-1))
                    else findTv tvs (n+1)
        in
            findTv argTypes (~nTypeVars)
        end

        val resultCode = makeEq(resType, MakeFun, getEqFnForID, typeVarFun)
        (* We need a function that takes two arguments rather than a single pair. *)
        val innerFnCode =
            case resultCode of
                PairArg c =>
                    mkInlproc(mkEval(c (level+2), [arg1, arg2], true),
                              level+1, 1, "equality")
            |   TwoArgs c => c(level+1)
    in
        mkInlproc(innerFnCode, level, nTypeVars, "equality()")
    end

    and equalityForType(ty: types, level: int): codetree =
    let
        fun getEqFnForID(typeId, _, l) = (mkInd(0, codeId(typeId, l)), NONE)
        fun unmatchedTypeVar _ = PairArg(fn _ => mkConst (toMachineWord structureEq))
        val resultCode = makeEq(ty, MakeFun, getEqFnForID, unmatchedTypeVar)
    in
        (* The final result function must take a single argument.  If we have
           generated a function the result must be one which takes two arguments.
           If we have not generated it it must have come from somewhere else so
           it must take a pair. *)
        case resultCode of
            PairArg c => c level
        |   TwoArgs c =>
                (* We need to wrap this up in a new inline function. *)
                mkInlproc(mkEval(c (level+1), [mkInd(0, arg1), mkInd(1, arg1)], true),
                          level, 1, "equality")
    end

    (* Create a printer for a given type.  This has type 'a * int -> pretty.
       The easiest way is to use printerForTypeFunction and apply the
       function to an empty set of type arguments.
       fn (v, depth) => ptf depth () v
       *)
    and printerForType (ty: types, level): codetree =
        mkProc(
            mkEval(
                mkEval(
                    mkEval(
                        printerForTypeFunction([], ty, level+1),
                        [mkInd(1, mkLoad(~1, 0))], (* Depth *)
                        false
                    ),
                    [CodeZero], (* Type arguments. *)
                    false
                ),
                [mkInd(0, mkLoad(~1, 0))], (* Value to print. *)
                false
            ),
            level, 1, "print-helper")

    (* Exported version. *)
    val printerForType = fn (ty, level) => printerForType(ty, level)

    (* Exported function.  Returns a function from an ML pair of values to bool.
       N.B. This differs from the functions in the typeID which take a Poly pair. *)
    val equalityForType = fn (ty, level) => equalityForType(ty, level)

    (* Create equality functions for a set of possibly mutually recursive datatypes. *)
    fun equalityForDatatypes(typelist, eqAddresses, baseLevel): codetree list =
    let
        val typesAndAddresses = ListPair.zipEq(typelist, eqAddresses)

        fun equalityForDatatype(tyConstr, addr) =
        if tcEquality tyConstr
        then
        let
            val argTypes = tcTypeVars tyConstr
            (* If this is a reference to a datatype we're currently generating
               load that address otherwise fall back to the default. *)
            fun getEqFnForID(typeId, actualArgs, l) =
            let
                val code =
                    if sameTypeId(typeId, tcIdentifier tyConstr)
                    then mkLoad(0, l-baseLevel-1) (* Directly recursive. *)
                    else
                    case List.find(fn(tc, addr) => sameTypeId(tcIdentifier tc, typeId)) typesAndAddresses of
                        SOME(_, addr) => mkLoad(addr, l-baseLevel) (* Mutually recursive. *)
                    |   NONE => mkInd(0, codeId(typeId, l))
                (* If this is a recursive call and the type arguments that are being passed (if any) are
                   the same as the original arguments we can call the inner function directly.
                   e.g. if we have datatype 'a list = nil | :: of ('a * 'a list) the recursive
                   call has the same arguments but if we have datatype 'a t = X | Y of int t we
                   can't do this. *)
                val directRecursion =
                    if sameTypeId(typeId, tcIdentifier tyConstr) andalso
                            ListPair.foldlEq(fn (TypeVar tv, tv', true) => sameTv(tv, tv') | _ => false)
                                true (actualArgs, argTypes)
                    then SOME(mkLoad(0, l-baseLevel-2))
                    else NONE
            in
                (code, directRecursion)
            end

            (* Argument type variables. *)
            val nTypeVars = List.length argTypes
            fun typeVarFun tv =
            let
                fun findTv [] n = (* All type variables should be bound. *)
                        raise InternalError "Free type variable"
                 |  findTv (tv' :: tvs) n =
                        if sameTv(tv, tv')
                        then TwoArgs(fn l => mkLoad(n, l-baseLevel-1))
                        else findTv tvs (n+1)
            in
                findTv argTypes (~nTypeVars)
            end

            (* Filter out the EnumForm constructors.  They arise
               in situations such as datatype t = A of int*int | B | C
               i.e. where we have only one non-nullary constructor
               and it is a tuple.  In this case we can deal with all
               the nullary constructors simply by testing whether
               the two arguments are the same.  We don't have to
               discriminate the individual cases. *)
            fun isEnum(Value{class=Constructor{nullary=true, ...}, access=Global code, ...}) =
            let
                open ADDRESS
            in
                (* If the value is a short integer then we can check
                   for equality using pointer equality. *)
                isShort(loadWord(toAddress(evalue code), 0w1))
            end
            | isEnum _ = false

            fun processConstrs [] =
                (* The last of the alternatives is false *) CodeZero

            |  processConstrs ((vConstr as Value{class, access, typeOf, name=tempConstrName, ...}) ::rest) =
                if isEnum vConstr then processConstrs rest
                else
                let
                    val newLevel = baseLevel+2 (* We have two enclosing functions. *)
                    val base = codeAccess(access, newLevel)
                    fun matches arg =
                        mkEval(mkInd(0, base) (* Test function. *), [arg], true)
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
                            fun destruct argNo l =
                                mkEval(mkInd(2, codeAccess(access, l)) (* projection function. *),
                                    [mkLoad(argNo, l-newLevel)], true)

                            (* Test whether the values match. *)
                            val eqValue =
                                applyEq(resType, destruct ~1, destruct ~2, newLevel, getEqFnForID, typeVarFun)    
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
                mkProc(eqCode, baseLevel+1, 2, "eq-" ^ tcName tyConstr ^ "(2)")
            val code = mkProc(eqFun, baseLevel, nTypeVars, "equality()")
        in
            mkDec(addr, code)
        end
        else (* Not an equality type.  This will not be called. *)
            mkDec(addr, CodeZero)
    in
        List.map equalityForDatatype typesAndAddresses
    end

    (* Create a printer function for a datatype when the datatype is declared.
       We don't have to treat mutually recursive datatypes specially because
       this is called after the type IDs have been created. *)
    fun printerForDatatype(typeConstr, level) =
    let
        val name = tcName typeConstr
        val argTypes = tcTypeVars typeConstr
        val argCode = mkLoad(~1, 0)
        and depthCode = mkLoad(~1, 2)
        val innerLevel = level+3

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
                val constructorCode =
                    codeAccess(access, innerLevel)
                
                val printCode =
                    if nullary
                    then (* Just the name *) codePrettyString name
                    else
                    let
                        val typeOfArg =
                            case typeOf of
                                FunctionType{arg, ...} => arg
                            |   _ => raise InternalError "contructor not a function"
                        val getValue = mkEval(mkInd(2, constructorCode), [argCode], true)
                    in
                        codePrettyBlock(1, false, [],
                            codeList(
                                [
                                    codePrettyString name,
                                    codePrettyBreak (1, 0),
                                    (* Print the argument and parenthesise it if necessary. *)
                                    mkEval(mkConst(toMachineWord parenthesise),
                                        [printCodeForType(typeOfArg, getValue, depth, depthCode,
                                                     innerLevel, argTypes)],
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
                        val testValue = mkEval(mkInd(0, constructorCode), [argCode], true)
                    in
                        mkIf(testValue, printCode, printerForConstructors(rest, depth+1))
                    end,
                    codePrettyString "...")
            end

        |   printerForConstructors _ = raise InternalError ("No constructors:"^name)
            
        val printerCode = printerForConstructors(tcConstructors typeConstr, 0)
    in
        (* Wrap this in the functions for the depth and the type arguments. *)
        mkProc(
            mkProc(
                mkProc(printerCode, level+2, 1, "print-"^name),
                level+1, 1, "print"^name^"()"),
            level, 1, "print"^name^"()()")
    end    

    (* Opaque matching and functor application create new type IDs using an existing
       type as implementation.  The equality function is inherited whether the type
       was specified as an eqtype or not.  The print function is inherited but a new
       ref is created so that if a pretty printer is installed for the new type it
       does not affect the old type. *)
    (* If this is a type function we're going to generate a new ref anyway so we
       don't need to copy it. *)
    fun codeGenerativeId(sourceId as TypeFunction(argTypes, resType), isEq, level) =
        let
            val printCode = printerForTypeFunction(argTypes, resType, level)
            and eqCode =
                if isEq
                then equalityForTypeFunction(argTypes, resType, level)
                else CodeZero
        in
            mkTuple[
                eqCode,
                mkEval
                    (mkConst (ioOp POLY_SYS_alloc_store),
                    [mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags),
                     printCode],
                false)  
            ]
        end

    |   codeGenerativeId(sourceId, isEq, level) =
        let
            (* TODO: Use multipleUses here. *)
            val sourceCode = codeId(sourceId, level)
        in
            mkTuple
            [
                mkInd(0, sourceCode),
                mkEval
                    (mkConst (ioOp POLY_SYS_alloc_store),
                    [mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags),
                     mkEval(mkConst(ioOp POLY_SYS_load_word),
                        [mkInd(1, sourceCode), CodeZero], false)
                      ],
                false)  
            ]
        end

    (* Create the equality and type functions for a set of mutually recursive datatypes. *)
    fun createDatatypeFunctions(typelist, mkAddr, level) =
    let
        (* Each entry has an equality function and a ref to a print function.
           The print functions for each type needs to indirect through the refs
           when printing other types so that if a pretty printer is later
           installed for one of the types the others will use the new pretty
           printer.  That means that the code has to be produced in stages. *)
        (* Create the equality functions.  Because mutual decs can only be functions we
           can't create the typeIDs themselves as mutual declarations. *)
        val eqAddresses = List.map(fn _ => mkAddr()) typelist (* Make addresses for the equalities. *)
        val equalityFunctions =
            mkMutualDecs(equalityForDatatypes(typelist, eqAddresses, level))

        (* Create the typeId values and set their addresses.  The print function is
           initially set as zero. *)
        local
            fun makeTypeId(tc, eqAddr) =
            let
                val var = vaLocal(idAccess(tcIdentifier tc))
                val newAddr = mkAddr()
                val idCode =
                mkTuple
                [
                    mkLoad(eqAddr, 0),
                    mkEval
                        (mkConst (ioOp POLY_SYS_alloc_store),
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
                    mkConst (ioOp POLY_SYS_assign_word),
                    [mkInd(1, codeId(tcIdentifier tc, level)),
                              CodeZero, printerForDatatype(tc, level)],
                    false)
        in
            val printerCode = List.map setPrinter typelist
        end
    in
        equalityFunctions :: typeIdCode @ printerCode
    end

    (* This code is used when the type checker has to construct a unique monotype
       because a type variable has escaped to the top level.
       The equality code always returns true and the printer prints "?". *)
    fun codeForUniqueId() =
        mkConst(ADDRESS.toMachineWord((fn _ => fn _ => true, ref(fn _ => fn _ => fn _ => PrettyString "?"))))


    structure Sharing =
    struct
        type typeId     = typeId
        type codetree   = codetree
        type types      = types
        type typeConstrs= typeConstrs
        type typeVarForm=typeVarForm
    end
end;
