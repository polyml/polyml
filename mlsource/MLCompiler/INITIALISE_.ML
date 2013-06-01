(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Updated David C.J. Matthews 2008-9, 2012, 2013

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

(*
    Title:      Initialise ML Global Declarations.
    Author:     Dave Matthews,Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

functor INITIALISE_ (

structure LEX: LEXSIG
structure TYPETREE : TYPETREESIG;
structure STRUCTVALS : STRUCTVALSIG;
structure VALUEOPS : VALUEOPSSIG;
structure CODETREE : CODETREESIG
structure EXPORTTREE: EXPORTTREESIG
structure DATATYPEREP: DATATYPEREPSIG
structure TYPEIDCODE: TYPEIDCODESIG

(*****************************************************************************)
(*                  MAKE                                                     *)
(*****************************************************************************)
structure MAKE :
sig
    type gEnv
    type env

    type values;
    type typeConstrSet;
    type fixStatus;
    type structVals;
    type signatures;
    type functors;

    type nameSpace =
    { 
        lookupVal:    string -> values option,
        lookupType:   string -> typeConstrSet option,
        lookupFix:    string -> fixStatus option,
        lookupStruct: string -> structVals option,
        lookupSig:    string -> signatures option,
        lookupFunct:  string -> functors option,

        enterVal:     string * values      -> unit,
        enterType:    string * typeConstrSet -> unit,
        enterFix:     string * fixStatus   -> unit,
        enterStruct:  string * structVals  -> unit,
        enterSig:     string * signatures  -> unit,
        enterFunct:   string * functors    -> unit,

        allVal:       unit -> (string*values) list,
        allType:      unit -> (string*typeConstrSet) list,
        allFix:       unit -> (string*fixStatus) list,
        allStruct:    unit -> (string*structVals) list,
        allSig:       unit -> (string*signatures) list,
        allFunct:     unit -> (string*functors) list
    }
  
    val gEnvAsEnv    : gEnv -> env
    val gEnvAsNameSpace: gEnv -> nameSpace
    
    val useIntoEnv   : gEnv -> string -> unit

    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }

    type exportTree = EXPORTTREE.exportTree

    val compiler :
        nameSpace * (unit->char option) * Universal.universal list ->
        exportTree option * ( unit ->
           { fixes: (string * fixStatus) list, values: (string * values) list,
             structures: (string * structVals) list, signatures: (string * signatures) list,
             functors: (string * functors) list, types: (string * typeConstrSet) list }) option
end;

structure ADDRESS : AddressSig

structure DEBUG :
sig
  val lineNumberTag: (unit->int) Universal.tag
  val offsetTag: (unit->int) Universal.tag
  val fileNameTag: string Universal.tag
  val profilingTag  : int Universal.tag;
  val timingTag     : bool Universal.tag;
  val printDepthFunTag : (unit->int) Universal.tag;
  val errorDepthTag : int Universal.tag;
  val lineLengthTag : int Universal.tag;
  val profileAllocationTag : int Universal.tag
  
  val assemblyCodeTag        : bool Universal.tag;
  val parsetreeTag           : bool Universal.tag;
  val codetreeTag            : bool Universal.tag;
  val pstackTraceTag         : bool Universal.tag;
  val lowlevelOptimiseTag    : bool Universal.tag
  val codetreeAfterOptTag    : bool Universal.tag;
  val traceCompilerTag       : bool Universal.tag;
  val inlineFunctorsTag      : bool Universal.tag;
  val maxInlineSizeTag       : int Universal.tag;
  val debugTag                : bool Universal.tag;
  val reportUnreferencedIdsTag: bool Universal.tag;
  val reportExhaustiveHandlersTag: bool Universal.tag;
end;

structure MISC :
sig
  val unescapeString : string -> string
  exception Conversion of string;     (* string to int conversion failure *)
end;

structure DEBUGGER : DEBUGGERSIG
structure PRETTY : PRETTYSIG

structure VERSION:
sig
   val compilerVersion: string
   val versionNumber: int
end;

sharing STRUCTVALS.Sharing = VALUEOPS.Sharing = TYPETREE.Sharing = EXPORTTREE.Sharing
        = PRETTY.Sharing = CODETREE.Sharing = MAKE = ADDRESS = DATATYPEREP.Sharing
        = TYPEIDCODE.Sharing = DEBUGGER.Sharing = LEX.Sharing

) : 

(*****************************************************************************)
(*                  INITIALISE export signature                              *)
(*****************************************************************************)
sig
  type gEnv
  val initGlobalEnv : gEnv -> unit
end =

(*****************************************************************************)
(*                  INITIALISE functor body                                  *)
(*****************************************************************************)
struct
    open STRUCTVALS;
    open TYPETREE
    open VALUEOPS;
    open CODETREE;
    open ADDRESS;
    open MAKE;
    open MISC;
    open RuntimeCalls; (* for POLY_SYS calls *)
    open EXPORTTREE
    open DATATYPEREP
    
    val declInBasis = [DeclaredAt inBasis]

(*****************************************************************************)
(*                  Untility functions                                       *)
(*****************************************************************************)
    fun applyList _ []       = ()
    |   applyList f (h :: t) = (f h : unit; applyList f t);

(*****************************************************************************)
(*                  initGlobalEnv                                            *)
(*****************************************************************************)
    fun initGlobalEnv(globalTable  : gEnv) =
    let
        val Env globalEnv = MAKE.gEnvAsEnv globalTable
   
(*****************************************************************************)
(*                  Utilities                                                *)
(*****************************************************************************)        
        val enterGlobalValue  = #enterVal  globalEnv;
        val enterGlobalType   = #enterType globalEnv;

        (* Some routines to help make the types. *)
        local
            (* careful - STRUCTVALS.intType differs from TYPETREE.intType *)
            open TYPETREE;
        in
            (* Make some type variables *)
            fun makeEqTV  () = mkTypeVar (generalisable, true,  false, false)
            fun makeTV    () = mkTypeVar (generalisable, false, false, false)
            fun makePrintTV() = mkTypeVar (generalisable, false,  false, true)
            fun makeTypeVariable() =
                makeTv {value=emptyType, level=generalisable, equality=false,
                        nonunifiable=false, printable=false}
            
            (* Make some functions *)
            infixr 5 ->>
            fun a ->> b = mkFunctionType (a, b);
            
            infix 7 **;
            fun a ** b = mkProductType [a, b];
            
            (* Type identifiers for the types of the declarations. *)
            val Int    = intType;
            val String = stringType;
            val Bool   = boolType;
            val Unit   = unitType;
            val Char   = charType;
            val Word   = wordType;
            val Exn    = exnType
            
            val mkTypeConstruction = mkTypeConstruction;
        end;

        fun makePolymorphic(tvs, c) =
        let
            open TYPEIDCODE
            val tvs =
                List.filter(fn TypeVar tv => not justForEqualityTypes orelse tvEquality tv | _ => false) tvs
        in
            if null tvs then c else mkInlproc(c, List.length tvs, "", [], 0)
        end

        (* Function to make a type identifier with a pretty printer that just prints "?".
           None of the types are equality types so the equality function is empty. *)
        local
            fun monotypePrinter _ = PRETTY.PrettyString "?"
        in
            fun defaultEqAndPrintCode () =
                let
                    open TypeValue
                    val code =
                        createTypeValue{
                            eqCode = CodeZero, printCode = mkConst (toMachineWord (ref monotypePrinter)),
                            boxedCode = boxedEither (* Assume this for the moment *), sizeCode = singleWord }
                in
                    Global (genCode(code, [], 0) ())
                end
        end
        
       fun makeTypeAbbreviation(name, typeVars, typeResult, locations) =
            makeTypeConstructor(
                name, typeVars, makeTypeFunction(basisDescription name, (typeVars, typeResult)),
                locations)
  
        (* List of something *)
        fun List (base : types) : types =
            mkTypeConstruction ("list", tsConstr listConstr, [base], declInBasis);

        (* ref something *)
        fun Ref (base : types) : types  =
            mkTypeConstruction ("ref", refConstr, [base], declInBasis);
        
        fun Option (base : types) : types  =
            mkTypeConstruction ("option", tsConstr optionConstr, [base], declInBasis);
        
        
        (* Type-dependent functions. *)
        fun mkSpecialFun (name:string, typeof:types, opn: typeDependent) : values =
            makeOverloaded (name, typeof, opn);
        
        (* Overloaded functions. *)
        fun mkOverloaded (name:string) (typeof: types)
            : values = mkSpecialFun(name, typeof, TypeDep);
         
(*****************************************************************************)
(*                  unit                                                     *)
(*****************************************************************************)
        val () = enterGlobalType ("unit", TypeConstrSet(unitConstr, []));

(*****************************************************************************)
(*                  bool                                                     *)
(*****************************************************************************)
        local
            val falseCons =
                mkGconstr ("false", Bool,
                    createNullaryConstructor(EnumForm{tag=0w0, maxTag=0w1}, [], "false"), true, 2, declInBasis);
            val trueCons  =
                mkGconstr ("true",  Bool,
                    createNullaryConstructor(EnumForm{tag=0w1, maxTag=0w1}, [], "true"), true, 2, declInBasis);
        in
            val () = enterGlobalType  ("bool",  TypeConstrSet(boolConstr, [trueCons, falseCons]));
            val () = enterGlobalValue ("true",  trueCons);
            val () = enterGlobalValue ("false", falseCons);
        end;
        

(*****************************************************************************)
(*                  int                                                      *)
(*****************************************************************************)
        val () = enterGlobalType ("int", TypeConstrSet(intConstr, []));
   
(*****************************************************************************)
(*                  char                                                     *)
(*****************************************************************************)
        val () = enterGlobalType ("char", TypeConstrSet(charConstr, []));
   
(*****************************************************************************)
(*                  string                                                   *)
(*****************************************************************************)
        val () = enterGlobalType ("string", TypeConstrSet(stringConstr, []));

        (* chr - define it as an identity function for now. It is redefined in
           the prelude to check that the value is a valid character. *)
        local
            val chrCode = identityFunction "chr";
            val chrType = Int ->> String;
            val chrVal  = mkGvar ("chr", chrType, chrCode, declInBasis);
        in
            val () = enterGlobalValue ("chr", chrVal);
        end;        
    
(*****************************************************************************)
(*                  real                                                     *)
(*****************************************************************************)
        val () = enterGlobalType ("real", TypeConstrSet(realConstr, []));

(*****************************************************************************)
(*                  'a list                                                  *)
(*****************************************************************************)
        val () = (* Enter :: and nil. *)
            List.app(fn(tv as Value{name, ...}) => enterGlobalValue(name, tv))
                (tsConstructors listConstr)
        val () = enterGlobalType  ("list", listConstr);

(*****************************************************************************)
(*                  'a option                                                  *)
(*****************************************************************************)
        val () = (* Enter NONE and SOME. *)
            List.app(fn(tv as Value{name, ...}) => enterGlobalValue(name, tv))
                (tsConstructors optionConstr)
        val () = enterGlobalType  ("option", optionConstr);

(*****************************************************************************)
(*                  ref                                                      *)
(*****************************************************************************)
        local
            val refCons =
                let
                    val a : types = makeTV ();
                in
                    mkGconstr ("ref", a ->> Ref a,
                        createUnaryConstructor(RefForm, [a], "ref"), false, 1, declInBasis)
                end
        in
            val () = enterGlobalType  ("ref", TypeConstrSet(refConstr, [refCons]));
            val () = enterGlobalValue ("ref", refCons);
        end;

        (* '!' does not really have to be here but it makes it easier
           to ensure that it is implemented in-line. *)
        local
            val plingCode = (* we load the zero'th word from the parameter *)
                mkInlproc
                    (mkEval (rtsFunction POLY_SYS_load_word, [mkLoadArgument 0, CodeZero]), 1, "!(1)", [], 0)
            val a = makeTV ()
            val plingType = Ref a ->> a
            val plingVal  = mkGvar ("!", plingType, makePolymorphic([a], plingCode), declInBasis);
        in
            val () = enterGlobalValue ("!", plingVal);
        end;        

(*****************************************************************************)
(*                  exn                                                      *)
(*****************************************************************************)
        val () = enterGlobalType ("exn", TypeConstrSet(exnConstr, []));

(*****************************************************************************)
(*                  word                                                      *)
(*****************************************************************************)
        val () = enterGlobalType ("word", TypeConstrSet(wordConstr, []));


(*****************************************************************************)
(*                  System functions (in structure RunCall)                  *)
(*****************************************************************************)
        local
            val runCall = makeEmptyGlobal "RunCall";
        in
            val ()        = #enterStruct globalEnv ("RunCall", runCall);
            val (Env runCallEnv) = makeEnv (sigTab(structSignat runCall));
        end;
        
        fun enterRunCall (name : string, entry : codetree, typ : types) : unit =
        let
            val value = mkGvar (name, typ, entry, declInBasis);
        in
            #enterVal runCallEnv (name, value)
        end;


(*****************************************************************************)
(*                  RunCall.unsafeCast                                        *)
(*****************************************************************************)
  
        local
            val a = makeTV ();
            val b = makeTV ();
            val unsafeCastType = a ->> b;

            val unsafeCastEntry : codetree =
            let 
                val name = "unsafeCast(1)";
                val args  = 1;
                val body  = mkLoadArgument 0  (* just the parameter *)
            in
                mkInlproc (body, args, name, [], 0)
            end;
        in
            val () =
                enterRunCall ("unsafeCast", makePolymorphic([a, b], unsafeCastEntry), unsafeCastType);
        end;
        

(*****************************************************************************)
(*                  RunCall.run_call*                                         *)
(*****************************************************************************)

        local
            val a = makeTV ();
            val b = makeTV ();
            val c = makeTV ();
            val d = makeTV ();
            val e = makeTV ();
            val f = makeTV ();
            val runCall0Type = Int ->> Unit ->> a;
            val runCall1Type = Int ->> a ->> b;
            val runCall2Type = Int ->> TYPETREE.mkProductType [a,b] ->> c;
            val runCall3Type = Int ->> TYPETREE.mkProductType [a,b,c] ->> d;
            val runCall4Type = Int ->> TYPETREE.mkProductType [a,b,c,d] ->> e;
            val runCall5Type = Int ->> TYPETREE.mkProductType [a,b,c,d,e] ->> f;
            val runCall2C2Type = Int ->> TYPETREE.mkProductType [a,b] ->> TYPETREE.mkProductType [c,d]

            (* 
               We used to have the following definition:
            
                 val runCall1Entry = mkEntry POLY_SYS_io_operation;
                
               but it didn't work as well, because CODETREE.ML wouldn't optimise
               expressions like:
               
                 RunCall.run_call1 POLY_SYS_io_operation
                 
               because there was nothing to tell it that this should be evaluated
               "early". Now we use an inline procedure wrapped round the constant,
               and set the "early" flag in the inline proc. SPF 2/5/95.
            *)

            val runCall1Entry : codetree =
            let 
                val name = "run_call1(1)";
                val ioOpEntry = rtsFunction POLY_SYS_io_operation;
                val n     = mkLoadArgument 0                 (* the outer parameter *)
                val body  = mkEval (ioOpEntry, [n]);
            in
                makePolymorphic([a, b], mkInlproc (body, 1, name, [], 0))
            end;

            fun makeRunCallTupled (width:int) : codetree =
            let 
              (* These declarations should really be read in the reverse order.
                 We are trying to build the codetree for something like the
                 following:
                 
                    val run_call3 = 
                      fn (n:int) => 
                      let
                         val f = ioOp n
                      in
                         fn (x,y,z) => f <x,y,z>
                      end;
                      
                 where "f <x,y,z>" designates Poly-style (values in registers)
                 uncurried parameter passing.
                 *)

                val name = "run_call" ^ Int.toString width;
                val ioOpEntry = rtsFunction POLY_SYS_io_operation;
                
                val innerBody : codetree =
                let
                    val f     = mkLoadClosure 0        (* first item from enclosing scope *)
                    val tuple = mkLoadArgument 0       (* the inner parameter *)
                    val args  = List.tabulate(width, fn n => mkInd (n, tuple))
                in
                    mkEval (f, args)
                end

                val innerLambda = mkInlproc (innerBody, 1, name ^ "(1)", [mkLoadLocal 0], 0)
                val outerBody : codetree  =
                let
                    val n = mkLoadArgument 0                 (* the outer parameter *)
                    val f = mkEval (ioOpEntry, [n]);
                in
                    mkEnv([mkDec (0, f)], innerLambda)
                end

                val outerLambda  = mkInlproc (outerBody, 1, name, [], 1)
            in
                outerLambda
            end

            (* Versions that return the results in a container i.e. a tuple on the stack.
               Currently this is only used for quotrem. *)
            fun makeRunCallTupledWithContainer (width:int, containerWidth) : codetree =
            let 
                val name =
                    String.concat["run_call", Int.toString width, "C", Int.toString containerWidth]
                val ioOpEntry = rtsFunction POLY_SYS_io_operation
                
                val innerLambda : codetree =
                let
                    val f     = mkLoadClosure 0        (* first item from enclosing scope *)
                    val tuple = mkLoadArgument 0       (* the inner parameter *)
                    val ca = 0 (* Address of container *)
                    val args  =
                        List.tabulate(width, fn n => mkInd (n, tuple)) @ [mkLoadLocal ca]
                in
                    mkInlproc(
                        mkEnv([mkContainer(ca, containerWidth, mkEval (f, args))],
                              mkTupleFromContainer(ca, containerWidth)),
                        1, name ^ "(1)", [mkLoadLocal 0], 1)
                end

                val outerLambda =
                let
                    val n = mkLoadArgument 0                 (* the outer parameter *)
                    val f = mkEval (ioOpEntry, [n])
                in
                    mkInlproc(mkEnv([mkDec (0, f)], innerLambda), 1, name, [], 1)
                end
            in
                outerLambda
            end
            
            val runCall0Entry = makePolymorphic([a], makeRunCallTupled 0);
            val runCall2Entry = makePolymorphic([a, b, c], makeRunCallTupled 2);
            val runCall3Entry = makePolymorphic([a, b, c, d], makeRunCallTupled 3);
            val runCall4Entry = makePolymorphic([a, b, c, d, e], makeRunCallTupled 4);
            val runCall5Entry = makePolymorphic([a, b, c, d, e, f], makeRunCallTupled 5);
            val runCall2C2Entry =
                makePolymorphic([a, b, c, d], makeRunCallTupledWithContainer(2, 2));
        in
            val () = enterRunCall ("run_call0", runCall0Entry, runCall0Type);
            val () = enterRunCall ("run_call1", runCall1Entry, runCall1Type);
            val () = enterRunCall ("run_call2", runCall2Entry, runCall2Type);
            val () = enterRunCall ("run_call3", runCall3Entry, runCall3Type);
            val () = enterRunCall ("run_call4", runCall4Entry, runCall4Type);
            val () = enterRunCall ("run_call5", runCall5Entry, runCall5Type);
            val () = enterRunCall ("run_call2C2", runCall2C2Entry, runCall2C2Type);
        end;
        
(*****************************************************************************)
(*                  Run-time exceptions in RunCall                           *)
(*****************************************************************************)
        
        local
            (* Create nullary exception. *)
            fun makeException0(name, id) =
            let
                val exc =
                    Value{ name = name, typeOf = TYPETREE.exnType,
                           access = Global(mkConst(toMachineWord id)),
                           class = Exception, locations = declInBasis,
                           references = NONE, instanceTypes=NONE }
            in
                #enterVal runCallEnv (name, exc)
            end
            (* Create exception with parameter. *)
            and makeException1(name, id, exType) =
            let
                val exc =
                    Value{ name = name, typeOf = exType ->> TYPETREE.exnType,
                           access = Global(mkConst(toMachineWord id)),
                           class = Exception, locations = declInBasis,
                           references = NONE, instanceTypes=NONE }
            in
                #enterVal runCallEnv (name, exc)
            end
        in
            val () = List.app makeException0
                [
                    ("Interrupt",   EXC_interrupt),
                    ("Size",        EXC_size),
                    ("Bind",        EXC_Bind),
                    ("Div",         EXC_divide),
                    ("Match",       EXC_Match),
                    ("Overflow",    EXC_overflow),
                    ("Subscript",   EXC_subscript)
                 ]
             val () = List.app makeException1
                [
                    ("Fail",        EXC_Fail,           String),
                    ("Conversion",  EXC_conversion,     String),
                    ("XWindows",    EXC_XWindows,       String),
                    ("Foreign",     EXC_foreign,        String),
                    ("Thread",      EXC_thread,         String),
                    ("SysErr",      EXC_syserr,         String ** Option Int),
                    ("ExTrace",     EXC_extrace,        List String ** Exn)
                ]
        end

(*****************************************************************************)
(*                  Bootstrapping functions (in structure Bootstrap)         *)
(*****************************************************************************)
        local
            val bootstrap = makeEmptyGlobal "Bootstrap";
        in
            val ()        = #enterStruct globalEnv ("Bootstrap", bootstrap);
            val (Env bootstrapEnv) = makeEnv (sigTab(structSignat bootstrap));
        end;
        
        fun enterBootstrap (name : string, entry : codetree, typ : types) : unit =
        let
            val value = mkGvar (name, typ, entry, declInBasis);
        in
            #enterVal bootstrapEnv (name, value)
        end;

(*****************************************************************************)
(*                  Initialisation and bootstrapping functions               *)
(*****************************************************************************)
        local
            fun addVal (name : string, value : 'a, typ : types) : unit =
                enterBootstrap (name, mkConst (toMachineWord value), typ)
      
            (* These are only used during the bootstrap phase.  Replacements are installed once
               the appropriate modules of the basis library are compiled. *)
            fun intOfString s =
                let
                val radix =
                    if String.size s >= 3 andalso String.substring(s, 0, 2) = "0x"
                       orelse String.size s >= 4 andalso String.substring(s, 0, 3) = "~0x"
                    then StringCvt.HEX else StringCvt.DEC
                in
                    case StringCvt.scanString (Int.scan radix) s of
                        NONE => raise Conversion "Invalid integer constant"
                      | SOME res => res
                end
        
            fun wordOfString s =
                let
                val radix =
                    if String.size s > 2 andalso String.sub(s, 2) = #"x"
                    then StringCvt.HEX else StringCvt.DEC
                in
                    case StringCvt.scanString (Word.scan radix) s of
                        NONE => raise Conversion "Invalid word constant"
                      | SOME res => res
                end
        in
            (* When we start the compiler we don't have any conversion functions.
               We can't even use a literal string until we have installed a
               basic converter. *)
            val () = addVal ("convStringName", "convString": string, String);
            val () = addVal ("convInt", intOfString : string -> int, String ->> Int);
            val () = addVal ("convWord", wordOfString : string -> word, String ->> Word);
            (* Convert a string, recognising and converting the escape codes. *)
            val () = addVal ("convString", unescapeString: string -> string, String ->> String);

        end;

(*****************************************************************************)
(*                  eqtypes                                                  *)
(*****************************************************************************)
    (* The only reason we have vector here is to get equality right.  We need
       vector to be an equality type and to have a specific equality function. *)
        local
            fun polyTypePrinter _ _ = PRETTY.PrettyString "?"
            (* The equality function takes the base equality type as an argument.
               The inner function takes two arguments which are the two vectors to
               compare, checks the lengths and if they're equal applies the
               base equality to each field. *)
            val eqCode =
                mkInlproc(
                    mkProc(
                        mkEnv([
                            (* Length of the items. *)
                            mkDec(0, mkEval(rtsFunction POLY_SYS_get_length, [mkLoadArgument 0])),
                            mkDec(1, mkEval(rtsFunction POLY_SYS_get_length, [mkLoadArgument 1])),
                            mkMutualDecs[(2, (* Loop function. *)
                                mkProc(
                                    mkIf(
                                        (* Finished? *)
                                        mkEval(rtsFunction POLY_SYS_word_eq, [mkLoadClosure 0, mkLoadArgument 0]),
                                        CodeTrue, (* Yes, all equal. *)
                                        mkIf(
                                            mkEval(
                                                TypeValue.extractEquality(mkLoadClosure 2), (* Base equality fn *)
                                                [
                                                    mkEval(rtsFunction POLY_SYS_load_word,
                                                        [mkLoadClosure 3, mkLoadArgument 0]),
                                                    mkEval(rtsFunction POLY_SYS_load_word,
                                                        [mkLoadClosure 4, mkLoadArgument 0])
                                                ]),
                                            mkEval(mkLoadClosure 1, (* Recursive call with index+1. *)
                                                [
                                                    mkEval(rtsFunction POLY_SYS_plus_word,
                                                        [mkLoadArgument 0, mkConst(toMachineWord 1)])
                                                ]),
                                            CodeFalse (* Not equal elements - result false *)
                                        )
                                    ),
                                1, "vector-loop",
                                    [mkLoadLocal 0 (* Length *), mkLoadLocal 2 (* Loop function *), 
                                     mkLoadClosure 0 (* Base equality function *), 
                                     mkLoadArgument 0 (* Vector 0 *), mkLoadArgument 1 (* Vector 1 *)], 0))]
                            ],
                            mkIf(
                                (* Test the lengths. *)
                                mkEval(rtsFunction POLY_SYS_word_eq, [mkLoadLocal 0, mkLoadLocal 1]),
                                (* Equal - test the contents. *)
                                mkEval(mkLoadLocal 2, [CodeZero]),
                                CodeFalse (* Not same length- result false *)
                            )
                        ),
                        2, "vector-eq", [mkLoadArgument 0], 3),
                    1, "vector-eq()", [], 0)

            val idCode = (* Polytype *)
                let
                    open TypeValue
                    val code =
                        createTypeValue{
                            eqCode=eqCode, printCode=mkConst (toMachineWord (ref polyTypePrinter)),
                            boxedCode=mkInlproc(boxedAlways, 1, "boxed-vector", [], 0),
                            sizeCode=mkInlproc(singleWord, 1, "size-vector", [], 0)}
                in
                    Global (genCode(code, [], 0) ())
                end
        in
            val vectorType =
                makeTypeConstructor("vector", [makeTypeVariable()],
                    makeFreeId(idCode, true, basisDescription "vector"), declInBasis);
            val () = enterGlobalType  ("vector", TypeConstrSet(vectorType, []));
        end

        (* We also need a type with byte-wise equality. *)
        local
            fun monoTypePrinter _ = PRETTY.PrettyString "?"
            (* This is a monotype equality function that takes two byte vectors and compares them
               byte-by-byte for equality.  Because they are vectors of bytes it's unsafe to load
               the whole words which could look like addresses if the bottom bit happens to be zero. *)
            val eqCode =
                mkProc(
                    mkEnv([
                        (* Length of the items. *)
                        mkDec(0, mkEval(rtsFunction POLY_SYS_get_length, [mkLoadArgument 0])),
                        mkDec(1, mkEval(rtsFunction POLY_SYS_get_length, [mkLoadArgument 1]))
                        ],
                        mkIf(
                            (* Test the lengths. *)
                            mkEval(rtsFunction POLY_SYS_word_eq, [mkLoadLocal 0, mkLoadLocal 1]),
                            (* Equal - test the contents. *)
                            mkEnv([
                                (* POLY_SYS_bytevec_eq takes a byte length so we have to multiply by
                                   the number of bytes per word. *)
                                mkDec(2,
                                    mkEval(rtsFunction POLY_SYS_mul_word,
                                        [mkEval(rtsFunction POLY_SYS_bytes_per_word, []), mkLoadLocal 0]
                                    ))
                                ],
                                mkEval(rtsFunction POLY_SYS_bytevec_eq,
                                    [mkLoadArgument 0, CodeZero, mkLoadArgument 1, CodeZero, mkLoadLocal 2])),
                            CodeFalse (* Not same length- result false *)
                        )
                    ),
                    2, "byteVector-eq", [], 3)

            val idCode = (* Polytype *)
                let
                    open TypeValue
                    val code =
                        createTypeValue{
                            eqCode=eqCode, printCode=mkConst (toMachineWord (ref monoTypePrinter)),
                            boxedCode=boxedAlways, sizeCode=singleWord}
                in
                    Global (genCode(code, [], 0) ())
                end
        in
            val byteVectorType =
                makeTypeConstructor("byteVector", [],
                    makeFreeId(idCode, true, basisDescription "byteVector"), declInBasis);
            val () = #enterType bootstrapEnv ("byteVector", TypeConstrSet(byteVectorType, []));
        end

        (* Similarly we need LargeWord.word *)
        local
            fun monoTypePrinter _ = PRETTY.PrettyString "?"

            val idCode =
                let
                    open TypeValue
                    val code =
                        createTypeValue{
                            eqCode=CODETREE.rtsFunction POLY_SYS_eq_longword,
                            printCode=mkConst (toMachineWord (ref monoTypePrinter)),
                            boxedCode = boxedNever,
                            sizeCode = singleWord
                            }
                in
                   Global (genCode(code, [], 0) ())
                end
        in
            val largeWordType =
                makeTypeConstructor("word", [],
                    makeFreeId(idCode, true, basisDescription "word"), declInBasis);
            (* This is put in Bootstrap so it can be picked out in the basis library.
               The default "word" type (Word.word) is in the global namespace. *)
            val () = #enterType bootstrapEnv ("word", TypeConstrSet(largeWordType, []));
        end

    (* We also need array and Array2.array to be passed through here so that
       they have the special property of being eqtypes even if their argument
       is not.   "array" is defined to be in the global environment. *)
        val () = enterGlobalType  ("array", TypeConstrSet(arrayConstr, []));
        val () = #enterType bootstrapEnv ("array", TypeConstrSet(array2Constr, []))
        val () = #enterType bootstrapEnv ("byteArray", TypeConstrSet(byteArrayConstr, []));

(*****************************************************************************)
(*                  Polymorphic functions                                    *)
(*****************************************************************************)
(* "=', '<>', PolyML.print etc are type-specific function which appear
   to be polymorphic.  The compiler recognises these and treats them specially.
   For (in)equality that means generating type-specific versions of the equality
   operations; for print etc that means printing in a type-specific way.  They
   can become true polymorphic functions and lose their type-specificity.  For
   (in)equality that means defaulting to structure equality which is normal and
   expected behaviour.  For print etc that means losing the ability to print
   and just printing "?" so it's important to avoid that happening.  "open"
   treats type-specific functions specially and retains the type-specificity.
   That's important to allow the prelude code to expand the PolyML structure. *)
        local
            val eqType = let val a = makeEqTV () in a ** a ->> Bool end;
            val eqVal  = mkSpecialFun("=", eqType, Equal);
        in
            val () = enterGlobalValue ("=", eqVal);
        end;        

        local
            val neqType = let val a = makeEqTV () in a ** a ->> Bool end;
            val neqVal  = mkSpecialFun("<>", neqType, NotEqual);
        in
            val () = enterGlobalValue ("<>", neqVal);
        end;        

(*****************************************************************************)
(*                  PolyML structure                                         *)
(*****************************************************************************)
        local
            val polyml = makeEmptyGlobal "PolyML";
        in
            val ()             = #enterStruct globalEnv ("PolyML", polyml);
            val (Env polyMLEnv) = makeEnv (sigTab(structSignat polyml));
            val enterPolyMLVal  = #enterVal polyMLEnv;
        end;

(*****************************************************************************)
(*                  Namespace functions                                      *)
(*****************************************************************************)
        local
        (* This version of the environment must match that used in the NameSpace
           structure. *)
            open TYPETREE
            (* Create a new structure for them. *)
            val nameSpace = makeEmptyGlobal "NameSpace";
            val _ = #enterStruct polyMLEnv ("NameSpace", nameSpace);
            val (Env nameSpaceEnv) = makeEnv (sigTab(structSignat nameSpace));
    
            (* Types for the basic values.  These are opaque. *)
            fun createType typeName =
            let
                val typeconstr =
                    makeTypeConstructor(typeName, [],
                        makeFreeId(defaultEqAndPrintCode(), false, basisDescription("PolyML.NameSpace." ^ typeName)),
                        declInBasis);
            in
                #enterType nameSpaceEnv (typeName, TypeConstrSet(typeconstr, []));
                mkTypeConstruction (typeName, typeconstr, [], declInBasis)
            end;
    
            val valueVal = createType "valueVal"
            val typeVal = createType "typeVal"
            val fixityVal = createType "fixityVal"
            val signatureVal = createType "signatureVal"
            val structureVal = createType "structureVal"
            val functorVal = createType "functorVal"
            
            (* nameSpace type.  Labelled record. *)
            fun createFields(name, vType): { name: string, typeof: types} list =
            let
                val enterFun = String ** vType ->> Unit
                val lookupFun = String ->> Option vType
                val allFun = Unit ->> List (String ** vType)
            in
                [mkLabelEntry("enter" ^ name, enterFun),
                 mkLabelEntry("lookup" ^ name, lookupFun),
                 mkLabelEntry("all" ^ name, allFun)]
            end
    
            (* We have to use the same names as we use in the env type because we're
               passing "env" values through the bootstrap. *)
            val valTypes = 
               [("Val", valueVal), ("Type", typeVal), ("Fix", fixityVal),
                ("Struct", structureVal), ("Sig", signatureVal), ("Funct", functorVal)];
    
            val fields = List.foldl (fn (p,l) => createFields p @ l) [] valTypes
    
            val recordType =
                makeTypeAbbreviation("nameSpace", [], mkLabelled(sortLabels fields, true), declInBasis);
            val () = #enterType nameSpaceEnv ("nameSpace", TypeConstrSet(recordType, []));
            
            (* The result type of the compiler includes valueVal etc. *)
            val resultFields = List.map TYPETREE.mkLabelEntry
                [("values", List(String ** valueVal)),
                 ("fixes", List(String ** fixityVal)),
                 ("types", List(String ** typeVal)),
                 ("structures", List(String ** structureVal)),
                 ("signatures", List(String ** signatureVal)),
                 ("functors", List(String ** functorVal))]
          in
            val nameSpaceType = mkTypeConstruction ("nameSpace", recordType, [], declInBasis)
            val execResult = mkLabelled(sortLabels resultFields, true)
            type execResult =
                { fixes: (string * fixStatus) list, values: (string * values) list,
                  structures: (string * structVals) list, signatures: (string * signatures) list,
                  functors: (string * functors) list, types: (string * typeConstrSet) list }

            val valueVal = valueVal
            val typeVal = typeVal
            val fixityVal = fixityVal
            val signatureVal = signatureVal
            val structureVal = structureVal
            val functorVal = functorVal
            
            val nameSpaceEnv = nameSpaceEnv
         end
         
        local
            val typeconstr = locationConstr
            val () = #enterType polyMLEnv ("location", typeconstr);
        in
            val Location = mkTypeConstruction ("location", tsConstr typeconstr, [], declInBasis)
        end

(*****************************************************************************)
(*                  context type                                           *)
(*****************************************************************************)
        local
            val typeconstr = contextConstr
        in
            val () = #enterType polyMLEnv ("context", typeconstr);
            val () = List.app(fn(tv as Value{name, ...}) => #enterVal polyMLEnv(name, tv))
                        (tsConstructors typeconstr)
        end

(*****************************************************************************)
(*                  pretty datatype (for printing)                           *)
(*****************************************************************************)
        local
            val typeconstr = prettyConstr
        in
            val () = #enterType polyMLEnv ("pretty", typeconstr);
            val () = List.app(fn(tv as Value{name, ...}) => #enterVal polyMLEnv(name, tv))
                        (tsConstructors typeconstr)
            val PrettyType = mkTypeConstruction ("pretty", tsConstr typeconstr, [], declInBasis)
        end


(*****************************************************************************)
(*              Funny polymorphic functions (in structure PolyML)            *)
(*****************************************************************************)
        local
            val printType = let val a = makePrintTV () in a ->> a end;
            val printVal  = mkSpecialFun("print", printType, Print);
        in
            val () = enterPolyMLVal ("print", printVal);
        end;

        local
            val makeStringType = let val a = makePrintTV () in a ->> String end;
            val makeStringVal  = mkSpecialFun("makestring", makeStringType, MakeString);
        in
            val () = enterPolyMLVal ("makestring", makeStringVal);
        end;

        local
            val prettyType = let val a = makePrintTV () in a ** Int ->> PrettyType end;
            val prettyVal  = mkSpecialFun("prettyRepresentation", prettyType, GetPretty);
        in
            val () = enterPolyMLVal ("prettyRepresentation", prettyVal);
        end;
 
        local
            (* addPrettyPrinter is the new function to install a pretty printer. *)
            val a = makeTV ()
            val b = makeTV ()
        
            val addPrettyType = (Int ->> b ->> a ->> PrettyType) ->> Unit;
            val addPrettyVal  = mkSpecialFun("addPrettyPrinter", addPrettyType, AddPretty);
        in
            val () = enterPolyMLVal ("addPrettyPrinter", addPrettyVal);
        end;

        local
            (* Old install_pp function to install a pretty-printer. *)
            val a = makeTV ()
            val b = makeTV ()
        
            val printTupleType =
                TYPETREE.mkProductType
                 [
                   String ->> Unit,      (* addString *)
                   Int ** Bool ->> Unit, (* beginBracket *)
                   Int ** Int ->> Unit,  (* space *)
                   Unit ->> Unit         (* endBracket *)
                 ];
            val installPPType = (printTupleType ->> Int ->> b ->> a ->> Unit) ->> Unit;
            val installPPVal  = mkSpecialFun("install_pp", installPPType, InstallPP);
        in
            val () = enterPolyMLVal ("install_pp", installPPVal);
        end;

        (* This goes in RunCall since it's only for the basis library. *)
        local
            val addOverloadType =
                let val a = makeTV () and b = makeTV () in (a ->> b) ->> String ->> Unit end;
            val addOverloadVal  = mkSpecialFun("addOverload", addOverloadType, AddOverload);
        in
            val () = #enterVal runCallEnv ("addOverload", addOverloadVal);
        end;

        local
            val sourceLocVal  = mkSpecialFun("sourceLocation", Unit ->> Location, GetLocation);
        in
            val () = enterPolyMLVal ("sourceLocation", sourceLocVal);
        end;

(*****************************************************************************)
(*                  Bootstrap.Universal                                      *)
(*****************************************************************************)
        local
            (* This is used as one of the arguments to the compiler function. *)
            open TYPETREE
            val uniStruct = makeEmptyGlobal "Universal";
            val _ = #enterStruct bootstrapEnv ("Universal", uniStruct);
            val (Env uniStructEnv) = makeEnv (sigTab(structSignat uniStruct));

            fun enterUniversal (name : string, entry : codetree, typ : types) : unit =
            let
                val value = mkGvar (name, typ, entry, declInBasis);
            in
                #enterVal uniStructEnv (name, value)
            end;

            local
                fun polyTypePrinter _ _ = PRETTY.PrettyString "?"
                open TypeValue
                val idCode =
                let
                    val code =
                        createTypeValue{
                                eqCode=CodeZero, (* Not an equality type *)
                                printCode=mkConst (toMachineWord (ref polyTypePrinter)),
                                boxedCode=mkInlproc(boxedEither(* Assume worst case *), 1, "boxed-tag", [], 0),
                                sizeCode=mkInlproc(singleWord, 1, "size-tag", [], 0)}
                in
                    Global (genCode(code, [], 0) ())
                end
            in
                (* type 'a tag *)
                val tagConstr =
                    makeTypeConstructor("tag", [makeTypeVariable()],
                        makeFreeId(idCode, false, basisDescription "tag"), declInBasis);
                val () = #enterType uniStructEnv ("tag", TypeConstrSet(tagConstr, []))
            end

            (* type universal *)
            val univConstr =
                makeTypeConstructor("universal", [],
                        makeFreeId(defaultEqAndPrintCode(), false, basisDescription "universal"), declInBasis);
            val () = #enterType uniStructEnv ("universal",  TypeConstrSet(univConstr, []));

            fun Tag base = mkTypeConstruction ("tag", tagConstr, [base], declInBasis)
            val Universal = mkTypeConstruction ("universal", univConstr, [], declInBasis)

            val a = makeTV()
            (* val tagInject  : 'a tag -> 'a -> universal *)
            val injectType = Tag a ->> a ->> Universal
            val () = enterUniversal ("tagInject",
                        makePolymorphic([a],
                            mkConst (toMachineWord (Universal.tagInject: 'a Universal.tag -> 'a -> Universal.universal))),
                            injectType)
            (* We don't actually need tagIs and tagProject since this is only used for
               the compiler.  Universal is redefined in the basis library. *)          
            val projectType = Tag a ->> Universal ->> a 
            val () = enterUniversal ("tagProject",
                        makePolymorphic([a],
                            mkConst (toMachineWord(Universal.tagProject: 'a Universal.tag -> Universal.universal -> 'a))),
                            projectType)
            val testType = Tag a ->> Universal ->> Bool
            val () = enterUniversal ("tagIs",
                        makePolymorphic([a],
                            mkConst (toMachineWord(Universal.tagIs: 'a Universal.tag -> Universal.universal -> bool))),
                            testType)
         in
            val Tag = Tag and Universal = Universal
        end
        
(*****************************************************************************)
(*                  parseTree type                                           *)
(*****************************************************************************)
        local
            open TYPETREE
            (* Parsetree properties datatype. *)
            val propConstr =
                makeTypeConstructor("ptProperties", [],
                    makeFreeId(defaultEqAndPrintCode(), false, basisDescription "PolyML.ptProperties"), declInBasis);
            val PtProperties = mkTypeConstruction ("ptProperties", propConstr, [], declInBasis)

            (* Parsetree type. *)
            val parseTreeConstr =
                makeTypeAbbreviation("parseTree", [], Location ** List PtProperties, declInBasis);    
            val ParseTree = mkTypeConstruction ("parseTree", parseTreeConstr, [], declInBasis)
            val () = #enterType polyMLEnv ("parseTree", TypeConstrSet(parseTreeConstr, []));

            (* Representation of the type of a value. *)
            val typesConstr =
                makeTypeConstructor("typeExpression", [],
                    makeFreeId(defaultEqAndPrintCode(), false, basisDescription "PolyML.typeExpression"), declInBasis);    
            val Types = mkTypeConstruction ("typeExpression", typesConstr, [], declInBasis)
            val () = #enterType polyMLEnv ("typeExpression", TypeConstrSet(typesConstr, []));

            val constrs = (* Order is significant. *)
               [ ("PTdeclaredAt",       Location),
                 ("PTfirstChild",       Unit ->> ParseTree),
                 ("PTnextSibling",      Unit ->> ParseTree),
                 ("PTopenedAt",         Location),
                 ("PTparent",           Unit ->> ParseTree),
                 ("PTpreviousSibling",  Unit ->> ParseTree),
                 ("PTprint",            Int ->> PrettyType),
                 ("PTreferences",       Bool ** List Location),
                 ("PTstructureAt",      Location),
                 ("PTtype",             Types)
                 ];
            (* This representation must match the representation defined in ExportTree.sml. *)
            val numConstrs = List.length constrs
            val {constrs=constrReps, ...} = chooseConstrRepr(constrs, [])
            val constructors =
                ListPair.map (fn ((s,t), code) =>
                    mkGconstr(s, t ->> PtProperties, code, false, numConstrs, declInBasis))
                        (constrs, constrReps)
            val () = List.app (fn c => #enterVal polyMLEnv(valName c, c)) constructors
            (* Put these constructors onto the type. *)
            val () = #enterType polyMLEnv ("ptProperties", TypeConstrSet(propConstr, constructors));

        in
            val ParseTree = ParseTree and Types = Types
        end

(*****************************************************************************)
(*        PolyML.compiler etc                                                *)
(*****************************************************************************)

        local
            open TYPETREE
 
            val compilerType : types =
                mkProductType[nameSpaceType, Unit ->> Option Char, List Universal] ->>
                    mkProductType[Option ParseTree, Option (Unit ->> execResult)]
            type compilerType =
                    nameSpace * (unit -> char option) * Universal.universal list -> exportTree option * (unit->execResult) option
        in
            val () = enterBootstrap ("use", mkConst (toMachineWord ((useIntoEnv globalTable): string -> unit)), String ->> Unit)            
            val () = enterPolyMLVal("compiler", mkGvar ("compiler", compilerType, mkConst (toMachineWord (compiler: compilerType)), declInBasis));
            val () = enterBootstrap("globalSpace", mkConst (toMachineWord(gEnvAsNameSpace globalTable: nameSpace)), nameSpaceType)
        end;

(*****************************************************************************)
(*                  Overloaded functions.                                    *)
(*****************************************************************************)
        
        local
            val ty      = TYPETREE.mkOverloadSet[]
            val addType = ty ** ty ->> ty;
            val negType = ty ->> ty;
            val cmpType = ty ** ty ->> Bool;
        in
            val () = enterGlobalValue ("+", mkOverloaded "+"   addType);
            val () = enterGlobalValue ("-", mkOverloaded "-"   addType);
            val () = enterGlobalValue ("*", mkOverloaded "*"   addType);
            val () = enterGlobalValue ("~", mkOverloaded "~"   negType);
            val () = enterGlobalValue ("abs", mkOverloaded "abs" negType);
            val () = enterGlobalValue (">=", mkOverloaded ">="  cmpType);
            val () = enterGlobalValue ("<=", mkOverloaded "<="  cmpType);
            val () = enterGlobalValue (">", mkOverloaded ">"   cmpType);
            val () = enterGlobalValue ("<", mkOverloaded "<"   cmpType);
            (* The following overloads are added in ML97 *)
            val () = enterGlobalValue ("div", mkOverloaded "div"   addType);
            val () = enterGlobalValue ("mod", mkOverloaded "mod"   addType);
            val () = enterGlobalValue ("/", mkOverloaded "/"   addType);
        end;
   
(*****************************************************************************)
(*                  Bootstrap entries copied from DEBUG                *)
(*****************************************************************************)
        local
            open DEBUG;
            val debuggerType =
                TYPETREE.mkProductType[Int, valueVal, Int, String, String, nameSpaceType] ->> Unit
            type debuggerType = int * values * int * string * string * nameSpace -> unit
            local
                open TYPETREE
                val fields =
                [
                    mkLabelEntry("location", Location), mkLabelEntry("hard", Bool),
                    mkLabelEntry("message", PrettyType), mkLabelEntry("context", Option PrettyType)
                ]
            in
                val errorMessageProcType = mkLabelled(sortLabels fields, true) ->> Unit
                type errorMessageProcType =
                    { location: location, hard: bool, message: pretty, context: pretty option } -> unit
            end

            local
                open TYPETREE
                val optNav = Option(Unit->>ParseTree)
                val fields =
                [
                    mkLabelEntry("parent", optNav),
                    mkLabelEntry("next", optNav),
                    mkLabelEntry("previous", optNav)
                ]
            in
                val navigationType = mkLabelled(sortLabels fields, true)
                type navigationType =
                    { parent: (unit->exportTree) option, next: (unit->exportTree) option, previous: (unit->exportTree) option }
            end
            type 'a tag = 'a Universal.tag
        in
            val () = applyList (fn (name, v, t) => enterBootstrap(name, mkConst v, t))
                [
                ("compilerVersion",        toMachineWord (VERSION.compilerVersion: string),     String),
                ("compilerVersionNumber",  toMachineWord (VERSION.versionNumber: int),          Int),
                ("lineNumberTag",          toMachineWord (lineNumberTag : (unit->int) tag),     Tag (Unit->>Int)),
                ("offsetTag",              toMachineWord (offsetTag: (unit->int) tag),          Tag (Unit->>Int)),
                ("fileNameTag",            toMachineWord (fileNameTag: string tag),             Tag String),
                ("maxInlineSizeTag",       toMachineWord (maxInlineSizeTag: int tag),           Tag Int),
                ("assemblyCodeTag",        toMachineWord (assemblyCodeTag: bool tag),           Tag Bool),
                ("parsetreeTag",           toMachineWord (parsetreeTag: bool tag),              Tag Bool),
                ("codetreeTag",            toMachineWord (codetreeTag: bool tag),               Tag Bool),
                ("pstackTraceTag",         toMachineWord (pstackTraceTag: bool tag),            Tag Bool),
                ("lowlevelOptimiseTag",    toMachineWord (lowlevelOptimiseTag: bool tag),       Tag Bool),
                ("codetreeAfterOptTag",    toMachineWord (codetreeAfterOptTag: bool tag),       Tag Bool),
                ("traceCompilerTag",       toMachineWord (traceCompilerTag: bool tag),          Tag Bool),
                ("inlineFunctorsTag",      toMachineWord (inlineFunctorsTag: bool tag),         Tag Bool),
                ("debugTag",               toMachineWord (debugTag: bool tag),                  Tag Bool),
                ("profilingTag",           toMachineWord (DEBUG.profilingTag: int tag),         Tag Int),
                ("timingTag",              toMachineWord (DEBUG.timingTag: bool tag),           Tag Bool),
                ("printDepthFunTag",       toMachineWord (DEBUG.printDepthFunTag: (unit->int) tag), Tag (Unit->>Int)),
                ("errorDepthTag",          toMachineWord (DEBUG.errorDepthTag: int tag),        Tag Int),
                ("lineLengthTag",          toMachineWord (DEBUG.lineLengthTag: int tag),        Tag Int),
                ("profileAllocationTag",   toMachineWord (DEBUG.profileAllocationTag: int tag), Tag Int),
                ("debuggerTag",            toMachineWord (DEBUGGER.debuggerFunTag: debuggerType tag), Tag debuggerType),
                ("printOutputTag",         toMachineWord (PRETTY.printOutputTag: (pretty->unit) tag),  Tag (PrettyType->>Unit)) ,               
                ("compilerOutputTag",      toMachineWord (PRETTY.compilerOutputTag: (pretty->unit) tag), Tag (PrettyType->>Unit)),
                ("errorMessageProcTag",    toMachineWord (LEX.errorMessageProcTag: errorMessageProcType tag), Tag errorMessageProcType),
                ("rootTreeTag",            toMachineWord (EXPORTTREE.rootTreeTag: navigation tag), Tag navigationType),
                ("reportUnreferencedIdsTag", toMachineWord (reportUnreferencedIdsTag: bool tag), Tag Bool),
                ("reportExhaustiveHandlersTag", toMachineWord (reportExhaustiveHandlersTag: bool tag), Tag Bool)
                ]
        end;
 

    (* PolyML.CodeTree structure.  This exports the CodeTree structure into the ML space. *)
        local
            open CODETREE
            val codetreeStr = makeEmptyGlobal "CodeTree"
            val _ = #enterStruct polyMLEnv ("CodeTree", codetreeStr)
            val (Env codetreeEnv) = makeEnv (sigTab(structSignat codetreeStr))

            fun createType typeName =
            let
                val typeconstr =
                    makeTypeConstructor(typeName, [],
                        makeFreeId(defaultEqAndPrintCode(), false, basisDescription("PolyML.CodeTree." ^ typeName)),
                        declInBasis);
            in
                #enterType codetreeEnv (typeName, TypeConstrSet(typeconstr, []));
                mkTypeConstruction (typeName, typeconstr, [], declInBasis)
            end

            val CodeTree = createType "codetree"
            and MachineWord = createType "machineWord"
            and CodeBinding = createType "codeBinding"

            (* For the moment export these only for the general argument and result types. *)
            fun simpleFn (code, nArgs, name, closure, nLocals) =
                mkFunction{body=code, argTypes=List.tabulate(nArgs, fn _ => GeneralType),
                           resultType=GeneralType, name=name, closure=closure, numLocals=nLocals}
            and simpleInlineFn (code, nArgs, name, closure, nLocals) =
                mkInlineFunction{body=code, argTypes=List.tabulate(nArgs, fn _ => GeneralType),
                           resultType=GeneralType, name=name, closure=closure, numLocals=nLocals}
            and simpleCall(func, args) =
                mkCall(func, List.map (fn c => (c, GeneralType)) args, GeneralType)
        in
            val CodeTree = CodeTree

            val () = applyList (fn (name, v, t) =>
                                #enterVal codetreeEnv (name, mkGvar (name, t, mkConst v, declInBasis)))
                [
                ("pretty", toMachineWord (CODETREE.pretty: codetree -> pretty), CodeTree ->> PrettyType),
                ("mkConstant", toMachineWord(mkConst: machineWord -> codetree), MachineWord ->> CodeTree),
                ("genCode", toMachineWord (genCode: codetree * Universal.universal list * int -> (unit->codetree)),
                        mkProductType[CodeTree, List Universal, Int] ->> (Unit ->> CodeTree)),
                ("evalue", toMachineWord (evalue: codetree -> machineWord option), CodeTree ->> Option MachineWord),
                ("mkFunction", toMachineWord (simpleFn: codetree * int * string * codetree list * int -> codetree),
                    mkProductType[CodeTree, Int, String, List CodeTree, Int] ->> CodeTree),
                ("mkInlineFunction", toMachineWord (simpleInlineFn: codetree * int * string * codetree list * int -> codetree),
                    mkProductType[CodeTree, Int, String, List CodeTree, Int] ->> CodeTree),
                ("mkCall", toMachineWord (simpleCall: codetree * codetree list -> codetree), CodeTree ** List CodeTree ->> CodeTree),
                ("mkLoadLocal", toMachineWord (mkLoadLocal: int -> codetree), Int ->> CodeTree),
                ("mkLoadArgument", toMachineWord (mkLoadArgument: int -> codetree), Int ->> CodeTree),
                ("mkLoadClosure", toMachineWord (mkLoadClosure: int -> codetree), Int ->> CodeTree),
                ("mkDec", toMachineWord (mkDec: int * codetree -> codeBinding), Int ** CodeTree ->> CodeBinding),
                ("mkInd", toMachineWord (mkInd: int * codetree -> codetree), Int ** CodeTree ->> CodeTree),
                ("mkIf", toMachineWord (mkIf: codetree * codetree * codetree -> codetree),
                    mkProductType[CodeTree, CodeTree, CodeTree] ->> CodeTree),
                ("mkWhile", toMachineWord (mkWhile: codetree * codetree -> codetree), CodeTree ** CodeTree ->> CodeTree),
                ("mkLoop", toMachineWord (mkLoop: codetree list -> codetree), List CodeTree ->> CodeTree),
                ("mkBeginLoop", toMachineWord (mkBeginLoop: codetree * (int * codetree) list -> codetree),
                    CodeTree ** List(Int ** CodeTree) ->> CodeTree),
                ("mkEnv", toMachineWord (mkEnv: codeBinding list * codetree -> codetree),
                    List CodeBinding ** CodeTree ->> CodeTree),
                ("mkMutualDecs", toMachineWord (mkMutualDecs: (int * codetree) list -> codeBinding),
                    List(Int ** CodeTree) ->> CodeBinding),
                ("mkTuple", toMachineWord (mkTuple: codetree list -> codetree), List CodeTree ->> CodeTree),
                ("mkRaise", toMachineWord (mkRaise: codetree -> codetree), CodeTree ->> CodeTree),
                ("mkHandle", toMachineWord (mkHandle: codetree * codetree -> codetree), CodeTree ** CodeTree ->> CodeTree),
                ("mkNullDec", toMachineWord (mkNullDec: codetree -> codeBinding), CodeTree ->> CodeBinding),
                ("Ldexc", toMachineWord (Ldexc: codetree), CodeTree),
                ("rtsFunction", toMachineWord (rtsFunction: int->codetree), Int ->> CodeTree)
                ]
        end

(*****************************************************************************)
(*                  Entries for printing                                     *)
(*****************************************************************************)
        local
            open TYPETREE
            (* These are used to display the declarations made. *)
            fun displayFix((name: string, f: fixStatus)): pretty =
            let
                open PRETTY
            in
                PrettyBlock (0, false, [],
                    [displayFixStatus f, PrettyBreak (1, 0), PrettyString name])
            end

            (* getValue can now be written in terms of codeForValue so it can probably be removed. *)
            fun getValue (Value{access = Global code, class = ValBound, ...}) = valOf(evalue code)
            |   getValue _ = raise Fail "Not a global value"

            (* The exported versions expect full name spaces as arguments.  Because we convert
               the exported versions to machineWord and give them types as data structures the
               compiler can't actually check that the type we give matched the internal type. *)
            fun makeTypeEnv(nameSpace: nameSpace): printTypeEnv =
            {
                lookupType = fn s => case #lookupType nameSpace s of NONE => NONE | SOME t => SOME(t, NONE),
                lookupStruct = fn s => case #lookupStruct nameSpace s of NONE => NONE | SOME t => SOME(t, NONE)
            }
            fun exportedDisplayTypeConstr(tyCons, depth, nameSpace: nameSpace) =
                TYPETREE.displayTypeConstrs(tyCons, depth, makeTypeEnv nameSpace)
            and exportedDisplayTypeExp(ty, depth, nameSpace: nameSpace) =
                TYPETREE.display(ty, depth, makeTypeEnv nameSpace)
            and exportedDisplaySigs(sign, depth, nameSpace: nameSpace) =
                    displaySignatures(sign, depth, makeTypeEnv nameSpace)
            and exportedDisplayFunctors(funct, depth, nameSpace: nameSpace) =
                    displayFunctors(funct, depth, makeTypeEnv nameSpace)
            and exportedDisplayValues(valu, depth, nameSpace: nameSpace) =
                    displayValues(valu, depth, makeTypeEnv nameSpace)
            and exportedDisplayStructs(str, depth, nameSpace: nameSpace) =
                    displayStructures(str, depth, makeTypeEnv nameSpace)
            and exportedPrintValue(v, depth, nameSpace: nameSpace) =
                    printValues(v, depth, makeTypeEnv nameSpace)

            fun codeForValue (Value{access = Global code, class = ValBound, ...}) = code
            |   codeForValue _ = raise Fail "Not a global value"
            and codeForStruct (Struct{access = Global code, ...}) = code
            |   codeForStruct _ = raise Fail "Not a global structure"
            and codeForFunct funct =
                case functorAccess funct of
                    Global code => code
                |   _ => raise Fail "Not a global functor"
        in
            (* Add these to the PolyML.NameSpace structure. *)
            val () = applyList (fn (name, v, t) =>
                                #enterVal nameSpaceEnv (name, mkGvar (name, t, mkConst v, declInBasis)))
                [
                ("displayFix",             toMachineWord (displayFix: string * fixStatus -> pretty),
                    String ** fixityVal ->> PrettyType),
                ("displaySig",             toMachineWord (exportedDisplaySigs: signatures * int * nameSpace -> pretty),
                    mkProductType[signatureVal, Int, nameSpaceType] ->> PrettyType),
                ("displayStruct",             toMachineWord (exportedDisplayStructs: structVals * int * nameSpace -> pretty),
                    mkProductType[structureVal, Int, nameSpaceType] ->> PrettyType),
                ("displayFunct",             toMachineWord (exportedDisplayFunctors: functors * int * nameSpace -> pretty),
                    mkProductType[functorVal, Int, nameSpaceType] ->> PrettyType),
                ("displayVal",             toMachineWord (exportedDisplayValues: values * int * nameSpace -> pretty),
                    mkProductType[valueVal, Int, nameSpaceType] ->> PrettyType),
                ("displayType",             toMachineWord (exportedDisplayTypeConstr: typeConstrSet * int * nameSpace -> pretty),
                    mkProductType[typeVal, Int, nameSpaceType] ->> PrettyType),
                (* displayTypeExpression doesn't really belong here since it's used
                   as part of the parse tree rather the name space. *)
                ("displayTypeExpression",    toMachineWord(exportedDisplayTypeExp: types * int * nameSpace -> pretty),
                    mkProductType[Types, Int, nameSpaceType] ->> PrettyType),
                (* Code functions *)
                ("codeForValue",    toMachineWord (codeForValue: values -> codetree), valueVal ->> CodeTree),
                ("codeForStruct",   toMachineWord (codeForStruct: structVals -> codetree), structureVal ->> CodeTree),
                ("codeForFunct",    toMachineWord (codeForFunct: functors -> codetree), functorVal ->> CodeTree)
               ]
            (* Put this in Bootstrap, at least for the moment.
               Used to print values in the debugger without the "val", "=" and the type. *)
           val () = applyList (fn (name, v, t) => enterBootstrap(name, mkConst v, t))
                [("printValue",             toMachineWord (exportedPrintValue: values * int * nameSpace -> pretty),
                    mkProductType[valueVal, Int, nameSpaceType] ->> PrettyType),
                 (* This is used to get the actual value out of a global "value".
                    It's currently used only in the debugger to get the exception
                    packet out of a global exception value. *)
                 ("getValue",               toMachineWord (getValue: values -> machineWord), valueVal ->> TYPETREE.exnType)]
        end;

    in
        ()
    end (* initGlobalEnv *);
end;
