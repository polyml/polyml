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
        val toMachineWord : 'a    -> machineWord;
        val F_words     : Word8.word;
        val F_bytes     : Word8.word;
        val F_mutable   : Word8.word;
    end;
    
    sharing LEX.Sharing = STRUCTVALS.Sharing = PRETTY.Sharing = CODETREE.Sharing
            = TYPETREE.Sharing = ADDRESS
) : TYPEIDCODESIG =
struct
    open CODETREE PRETTY ADDRESS STRUCTVALS
    open RuntimeCalls
    val InternalError = Misc.InternalError

    val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;

    val andb = Word8.andb and orb = Word8.orb
    infix 6 andb;
    infix 7 orb;
    val mutableFlags = F_words orb F_mutable;

    fun notFormal _ = raise InternalError "Not Formal"

    (* Temporary fall-back code. *)
    fun defaultEqAndPrintCode() =
    let
        (* The structure equality function takes a pair of arguments.  We need a
           function that takes two Poly-style arguments. *)
        val defaultEqCode =
            mkProc(
                mkProc(
                    mkEval(mkConst(toMachineWord structureEq),
                        [mkTuple[mkLoad(~1, 0), mkLoad(~2, 0)]], true), 1, 2, "eq-helper"),
                0, 0, "eq-helper()")
        fun defaultPrinter depth _ value = PrettyString "?"
        val code =
            mkTuple[
                defaultEqCode,
                mkConst (toMachineWord (ref defaultPrinter))
            ]
    in
        code
    end

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

    (* codeStruct and codeAccess are copied from ValueOps. *)
    fun codeStruct (str, level) =
        if isUndefinedStruct str
        then CodeNil
        else codeAccess (structAccess str, notFormal, level)

    and codeAccess (Global code, _, _) = code
      
    |   codeAccess (Local{addr=ref locAddr, level=ref locLevel}, _, level) =
            mkLoad (locAddr, level - locLevel) (* No need for the recursive case. *)
     
    |   codeAccess (Selected{addr, base}, _, level) =
            mkInd (addr, codeStruct (base, level))

    |   codeAccess (Formal addr, getFormal, level) = getFormal(addr, level)
     
    |   codeAccess _ = raise InternalError "No access"

    (* Load an identifier. *)
    fun codeIdWithFormal(Free{access=ref access, ...}, getFormal, level) = codeAccess(access, getFormal, level)
    |   codeIdWithFormal(Bound{access, ...}, getFormal, level) = codeAccess(access, getFormal, level)
    |   codeIdWithFormal(TypeFunction(argTypes, resType), getFormal, level) =
        let
            val printCode = printerForTypeFunction(argTypes, resType, getFormal, level)
            and eqCode = equalityForTypeFunction(argTypes, resType, getFormal, level)
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
    |   codeIdWithFormal _ = raise InternalError "codeId: Unknown form"

    (* Create a printer for a type function.  This is used to create the general
       print function for any type. *)
    and printerForTypeFunction(argTypes, resType, getFormal, level) =
        (* Wrap this in the functions for the depth and the type arguments. *)
        mkProc(
            mkProc(
                mkProc(
                    printCodeForType(
                        resType, mkLoad(~1, 0), 0, mkLoad(~1, 2), level+3, argTypes, getFormal),
                    level+2, 1, "print-helper"),
                level+1, 1, "print-helper()"),
            level, 1, "print-helper()()")

    and printCodeForType(ty, valToPrint, depth, depthCode, level, argTypes, getFormal) =
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
                            [_] => mkLoad(~1, 1)
                    |   _ => mkInd(position, mkLoad(~1, 1))
                in
                    mkEval(argCode, [mkTuple[valToPrint, adjustDepth(depthCode, depth)]], false)
                end
            end

        |   printCode(TypeConstruction { value, args, ...}, valToPrint, depth, depthCode, level) =
            let
                val typConstr = pling value
            in
                if tcIsAbbreviation typConstr (* Handle type abbreviations directly *)
                then printCode(TYPETREE.makeEquivalent (typConstr, args),
                               valToPrint, depth, depthCode, level)
                else
                let
                    val codedId = codeIdWithFormal(tcIdentifier typConstr, getFormal, level)
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
                            level, 1, "arg-printer")
                
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
        printCode(ty, valToPrint, depth, depthCode, level)
    end

    (* Create an equality function for a type function. *)
    and equalityForTypeFunction(argTypes, resType, getFormal, level) =
    let
    in
        mkProc(
            mkProc(
                mkEval(mkConst(toMachineWord structureEq),
                    [mkTuple[mkLoad(~1, 0), mkLoad(~2, 0)]], true), 1, 2, "eq-helper"),
            0, 0, "eq-helper()")
    end

    (* Create a printer for a given type.  This has type 'a * int -> pretty.
       The easiest way is to use printerForTypeFunction and apply the
       function to an empty set of type arguments.
       fn (v, depth) => ptf depth () v
       *)
    and printerForType (ty: types, getFormal, level): codetree =
        mkProc(
            mkEval(
                mkEval(
                    mkEval(
                        printerForTypeFunction([], ty, getFormal, level+1),
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

    (* As for printerForType, but this is simpler. *)
    fun equalityForType(ty: types, getFormal, level): codetree =
        mkEval(equalityForTypeFunction([], ty, getFormal, level), [], true)

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

        fun printerForConstructors
                (Value{name, typeOf, access, class = Constructor{nullary, ...}, ...} :: rest, depth) =
            let
                (* The "value" for a value constructor is a tuple containing
                   the test code, the injection and the projection functions. *)
                val constructorCode =
                    codeAccess(access, notFormal, innerLevel)
                val testValue = mkEval(mkInd(0, constructorCode), [argCode], true)
                val getValue = mkEval(mkInd(2, constructorCode), [argCode], true)
                val printCode =
                    if nullary
                    then (* Just the name *) codePrettyString name
                    else
                    let
                        val typeOfArg =
                            case typeOf of
                                FunctionType{arg, ...} => arg
                            |   _ => raise InternalError "contructor not a function"
                    in
                        codePrettyBlock(1, false, [],
                            codeList(
                                [
                                    codePrettyString name,
                                    codePrettyBreak (1, 0),
                                    printCodeForType(typeOfArg, getValue, depth, depthCode,
                                                     innerLevel, argTypes, notFormal)
                                ], CodeZero))
                    end
            in
                (* If this was the last or only constructor we don't need to test. *)
                checkDepth(depthCode, depth,
                    if null rest
                    then printCode
                    else mkIf(testValue, printCode, printerForConstructors(rest, depth+1)),
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
    fun codeGenerativeId(sourceId as TypeFunction(typeArgs, typeResult), getFormal, level) =
        codeIdWithFormal(sourceId, getFormal, level)

    |   codeGenerativeId(sourceId, getFormal, level) =
        let
            (* TODO: Use multipleUses here. *)
            val sourceCode = codeIdWithFormal(sourceId, getFormal, level)
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
    
    fun codeId(source, level) = codeIdWithFormal(source, notFormal, level)

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
        local
            fun createEquality(tc, addr) =
            let
                val code =
                if tcEquality tc
                then
                let
                    (* The structure equality function takes an argument pair.  We need a
                       function that takes two Poly-style arguments. *)
                    val defaultEqCode =
                        mkProc(
                            mkProc(
                                mkEval(mkConst(toMachineWord structureEq),
                                    [mkTuple[mkLoad(~1, 0), mkLoad(~2, 0)]], true), 1, 2, "eq-helper"),
                            0, 0, "eq-helper()")
                in
                    defaultEqCode
                end
                else (* The type system should ensure that this is never called. *)
                    mkConst(toMachineWord(fn _ => fn _ => raise Fail "Not an equality type"))
            in
                mkDec(addr, code)
            end
        in
            val equalityFunctions =
                mkMutualDecs(ListPair.map createEquality (typelist, eqAddresses))
        end

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

    structure Sharing =
    struct
        type typeId     = typeId
        type codetree   = codetree
        type types      = types
        type typeConstrs= typeConstrs
        type typeVarForm=typeVarForm
    end
end;
