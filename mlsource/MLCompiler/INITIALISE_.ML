(*
	Copyright (c) 2000
		Cambridge University Technical Services Limited

    Updated David C.J. Matthews 2008-9

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

(*****************************************************************************)
(*                  TYPETREE                                                 *)
(*****************************************************************************)
structure TYPETREE : TYPETREESIG;

(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS : STRUCTVALSIG;

(*****************************************************************************)
(*                  VALUEOPS                                                 *)
(*****************************************************************************)
structure VALUEOPS : VALUEOPSSIG;

(*****************************************************************************)
(*                  CODETREE                                                 *)
(*****************************************************************************)
structure CODETREE :
sig
  type machineWord
  type codetree
     
  val CodeNil:          codetree;
  val CodeZero:         codetree;
  val mkEnv:            codetree list -> codetree;
  val mkDec:            int * codetree -> codetree;
  val mkLoad:           int * int -> codetree;
  val mkInd:            int * codetree -> codetree;
  val mkConst:          machineWord -> codetree;
  val mkInlproc:        codetree * int * int * string -> codetree;
  val mkIf:             codetree * codetree * codetree -> codetree;
  val mkEval:           codetree * codetree list * bool -> codetree;
  val identityFunction: string -> codetree;
  val mkTuple:          codetree list -> codetree;
  val mkStr:            string   -> codetree;
  val mkRaise:          codetree -> codetree;

  val evalue:           codetree -> machineWord;
  val structureEq:      machineWord * machineWord -> bool;
end;

(*****************************************************************************)
(*                  EXPORTTREE                                               *)
(*****************************************************************************)

structure EXPORTTREE: EXPORTTREESIG

(*****************************************************************************)
(*                  MAKE                                                     *)
(*****************************************************************************)
structure MAKE :
sig
    type gEnv
    type env

    type values;
    type typeConstrs;
    type fixStatus;
    type structVals;
    type signatures;
    type functors;

    type nameSpace;
  
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
	         functors: (string * functors) list, types: (string * typeConstrs) list }) option
end;

(*****************************************************************************)
(*                  ADDRESS                                                  *)
(*****************************************************************************)
structure ADDRESS :
sig
  type machineWord
  val toMachineWord : 'a -> machineWord;
end;

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
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
  
  val assemblyCodeTag        : bool Universal.tag;
  val parsetreeTag           : bool Universal.tag;
  val codetreeTag            : bool Universal.tag;
  val pstackTraceTag         : bool Universal.tag;
  val codetreeAfterOptTag    : bool Universal.tag;
  val traceCompilerTag       : bool Universal.tag;
  val inlineFunctorsTag      : bool Universal.tag;
  val maxInlineSizeTag       : int Universal.tag;
  val debugTag				  : bool Universal.tag;
end;

(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
sig
  type 'a iter
  val unescapeString : string -> string
  val mapIterator : ('a -> 'b) -> 'a iter -> 'b list
  val upto : int -> int -> int iter
  exception Conversion of string;     (* string to int conversion failure *)
end;

(*****************************************************************************)
(*                  DEBUGGER                                                 *)
(*****************************************************************************)
structure DEBUGGER :
sig
    type values;
    type nameSpace;

    (* The debugger function supplied to the compiler. *)
    type debugger = int * values * int * string * string * nameSpace -> unit

    val debuggerFunTag : debugger Universal.tag
end;

(*****************************************************************************)
(*                  PRETTYPRINTER                                            *)
(*****************************************************************************)
structure PRETTY : PRETTYSIG

(*****************************************************************************)
(*                  VERSION                                                  *)
(*****************************************************************************)

structure VERSION:
sig
   val compilerVersion: string
end;

(*****************************************************************************)
(*                  INITIALISE sharing constraints                           *)
(*****************************************************************************)

sharing type
  ADDRESS.machineWord
= CODETREE.machineWord
    
sharing type
  CODETREE.codetree
= VALUEOPS.codetree
= STRUCTVALS.codetree

sharing type
  STRUCTVALS.values 
= VALUEOPS.values
= MAKE.values
  
sharing type
  TYPETREE.types
= STRUCTVALS.types 
= VALUEOPS.types
= EXPORTTREE.types

sharing type
  TYPETREE.typeConstrs
= STRUCTVALS.typeConstrs 
= MAKE.typeConstrs
= VALUEOPS.typeConstrs

sharing type
  STRUCTVALS.env
= MAKE.env

sharing type
  STRUCTVALS.functors
= MAKE.functors
= VALUEOPS.functors

sharing type
  STRUCTVALS.structVals
= MAKE.structVals
= VALUEOPS.structVals

sharing type
  STRUCTVALS.signatures
= MAKE.signatures
= VALUEOPS.signatures

sharing type
  STRUCTVALS.fixStatus
= MAKE.fixStatus
= VALUEOPS.fixStatus

sharing type
  VALUEOPS.pretty
= TYPETREE.pretty
= PRETTY.pretty
= EXPORTTREE.pretty

sharing type
  TYPETREE.locationProp
= STRUCTVALS.locationProp
= VALUEOPS.locationProp

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
    (* Don't open TYPETREE 'cos names clash with STRUCTVALS *)
    open STRUCTVALS;
    open VALUEOPS;
    open CODETREE;
    open ADDRESS;
    open MAKE;
    open MISC;
    open RuntimeCalls; (* for POLY_SYS calls *)
    open EXPORTTREE
    
    val declInBasis = [DeclaredAt inBasis]

(*****************************************************************************)
(*                  Untility functions                                       *)
(*****************************************************************************)
    fun applyList f []       = ()
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
        (* shouldn't these be imported from somewhere? *)
        fun ioOp (x: int) : 'a = RunCall.run_call1 POLY_SYS_io_operation x;
        val intZero    = mkConst (toMachineWord 0);
        val realZero   = mkConst (toMachineWord 0.0);
        
        fun loadArg n  = mkLoad (~ n, 0);
        fun mkEntry n  = mkConst (ioOp n);
        
        val enterGlobalValue  = #enterVal  globalEnv;
        val enterGlobalType   = #enterType globalEnv;

        (* Some routines to help make the types. *)
        local
            (* careful - STRUCTVALS.intType differs from TYPETREE.intType *)
            open TYPETREE;
        in
            (* Make some type variables *)
            fun makeEqTV  () = mkTypeVar (generalisable, true,  false, false);
            fun makeTV    () = mkTypeVar (generalisable, false, false, false);
            fun makeImpTV () = mkTypeVar (generalisable, false, false, true);
            
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
            
            val mkTypeConstruction = mkTypeConstruction;
            val mkTypeVar = mkTypeVar;
        end;
   
        (* List of something *)
        fun List (base : types) : types =
            mkTypeConstruction ("list", listType, [base], declInBasis);

        (* ref something *)
        fun Ref (base : types) : types  =
            mkTypeConstruction ("ref", refType, [base], declInBasis);

        local
            open ADDRESS PRETTY;

            fun defaultPrinter depth _ value = PrettyString "Unset printer"
            val monocode =
                mkTuple[
                    mkConst (toMachineWord structureEq),
                    mkConst (toMachineWord (ref defaultPrinter))
                ]
            (* Poly-types provide an extra argument to the equality function. *)
            val polycode =
                mkTuple[
                    mkConst (toMachineWord(fn _ => structureEq)),
                    mkConst (toMachineWord (ref defaultPrinter))
                ]
        in
            val monotypeCode = Global monocode
            val polytypeCode = Global polycode
        end
       
        (* option something *)
        val optionType = (* Can't freeze it because we add constructors *)
            makeDatatypeConstr("option", [makeTV()], makeFreeId(polytypeCode, true), 0, declInBasis);
        
        fun Option (base : types) : types  =
            mkTypeConstruction ("option", optionType, [base], declInBasis);
        
        
        (* Type-dependent functions. *)
        fun mkSpecialFun (name:string) (typeof:types) (opn: typeDependent) : values =
            makeOverloaded (name, typeof, opn);
        
        (* Overloaded functions. *)
        fun mkOverloaded (name:string) (typeof: types)
            : values = mkSpecialFun name typeof TypeDep;
         
(*****************************************************************************)
(*                  unit                                                     *)
(*****************************************************************************)
        val () = enterGlobalType ("unit", unitType);

(*****************************************************************************)
(*                  bool                                                     *)
(*****************************************************************************)
        local
            val falseCons =
                mkGconstr ("false", Bool,
                    createNullaryConstructor(EnumForm 0, "false"), true, 2, declInBasis);
            val trueCons  =
                mkGconstr ("true",  Bool,
                    createNullaryConstructor(EnumForm 1, "true"), true, 2, declInBasis);
        in
            val () = enterGlobalType  ("bool",  boolType);
            val () = enterGlobalValue ("true",  trueCons);
            val () = enterGlobalValue ("false", falseCons);
            
            (* Put these constructors onto the boolean type. *)
            val () =
                tcSetConstructors (boolType, [trueCons, falseCons]);
        end;
        

(*****************************************************************************)
(*                  int                                                      *)
(*****************************************************************************)
        val () = enterGlobalType ("int", intType);
   
(*****************************************************************************)
(*                  char                                                     *)
(*****************************************************************************)
        val () = enterGlobalType ("char", charType);
   
(*****************************************************************************)
(*                  string                                                   *)
(*****************************************************************************)
        val () = enterGlobalType ("string", stringType);

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
        val () = enterGlobalType ("real", realType);

(*****************************************************************************)
(*                  'a list                                                  *)
(*****************************************************************************)
        val () = enterGlobalType  ("list", listType);
        val () = enterGlobalValue ("::",   consConstructor);
        val () = enterGlobalValue ("nil",  nilConstructor);
        
        (* Put these constructors onto the list type. *)
        val () = tcSetConstructors (listType, [consConstructor, nilConstructor]);

(*****************************************************************************)
(*                  'a option                                                  *)
(*****************************************************************************)
        local
            val optionTypeVars  = tcTypeVars optionType;
            val alpha         = hd optionTypeVars;
            val alphaOption   = mkTypeConstruction ("option", optionType, optionTypeVars, declInBasis);
            val someType      = TYPETREE.mkFunctionType (alpha, alphaOption);
            (* These two representations are built into the RTS. *)
            val NoneForm = EnumForm 0;
            val SomeForm = BoxedForm;
            val noneConstructor  =
                mkGconstr ("NONE", alphaOption,
                    createNullaryConstructor(NoneForm, "NONE"),  true, 2, declInBasis);
            val someConstructor =
                mkGconstr ("SOME",  someType,
                    createUnaryConstructor(SomeForm, "SOME"), false, 2, declInBasis);
        in
            val () = enterGlobalType  ("option", optionType);
            val () = enterGlobalValue ("SOME",   someConstructor);
            val () = enterGlobalValue ("NONE",   noneConstructor);
            (* Put these constructors onto the option type.  N.B.  as with all
               value constructors these need to be in alphabetical order. *)
            val () = tcSetConstructors (optionType, [noneConstructor, someConstructor]);
        end;


(*****************************************************************************)
(*                  ref                                                      *)
(*****************************************************************************)
        local
            val refCons =
                let
                    val a : types = makeImpTV ();
                in
                    mkGconstr ("ref", a ->> Ref a,
                        createUnaryConstructor(RefForm, "ref"), false, 1, declInBasis)
                end
        in
            val () = enterGlobalType  ("ref", refType);
            val () = enterGlobalValue ("ref", refCons);
            
            (* Put this constructor onto the ref type. *)
            val () = tcSetConstructors (refType, [refCons]);
        end;

        (* '!' does not really have to be here but it makes it easier
           to ensure that it is implemented in-line. *)
        local
            val plingCode = (* we load the zero'th word from the parameter *)
                mkInlproc
                    (mkEval (mkConst (ioOp POLY_SYS_load_word), [(loadArg 1), CodeZero],
                        false) (* NOT early *), 0, 1, "!(1)");

            val plingType = let val a = makeTV () in Ref a ->> a end;
            val plingVal  = mkGvar ("!", plingType, plingCode, declInBasis);
        in
            val () = enterGlobalValue ("!", plingVal);
        end;        

(*****************************************************************************)
(*                  exn                                                      *)
(*****************************************************************************)
        val () = enterGlobalType ("exn", exnType);

(*****************************************************************************)
(*                  word                                                      *)
(*****************************************************************************)
        val () = enterGlobalType ("word", wordType);

(*****************************************************************************)
(*                  'a vector                                                *)
(*****************************************************************************)
    (* The only reason we have vector here is to get equality right.  We need
       vector to be an equality type and to use structure equality. *)
        val vectorType =
            makeFrozenTypeConstrs("vector", [makeTV()], makeFreeId(polytypeCode, true), 0, declInBasis);
        val () = enterGlobalType  ("vector", vectorType);


(*****************************************************************************)
(*                  System functions (in structure RunCall)                  *)
(*****************************************************************************)
        local
            val runCall = makeEmptyGlobal "RunCall";
        in
            val ()        = #enterStruct globalEnv ("RunCall", runCall);
            val (Env runCallEnv) = makeEnv (structSignat runCall);
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
                val level = 1;
                val args  = 1;
                val body  = mkLoad (~1, 0)  (* just the parameter *)
            in
                mkInlproc (body, level, args, name)
            end;
        in
            val () =
                enterRunCall ("unsafeCast", unsafeCastEntry, unsafeCastType);
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
                val ioOpEntry = mkEntry POLY_SYS_io_operation;
                val level = 1;
                val args  = 1;
                val n     = mkLoad (~1, 0)                 (* the outer parameter *)
                val body  = mkEval (ioOpEntry, [n], true); (* early! *)
            in
                mkInlproc (body, level, args, name)
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
                val ioOpEntry = mkEntry POLY_SYS_io_operation;
                
                val innerBody : codetree =
                let
                    val f     = mkLoad (1, 1);        (* first item from enclosing scope *)
                    val tuple = mkLoad (~1, 0);       (* the inner parameter *)
                    val args  = mapIterator (fn n => mkInd (n, tuple)) (upto 0 (width - 1));
                in
                    mkEval (f, args, false) (* no early evaluation (f may have side effects!) *)
                end;
                
                val innerLambda : codetree  =
                let
                    val level = 2;
                    val args  = 1;
                in
                    mkInlproc (innerBody, level, args, name ^ "(1)")
                end;
                
                val outerBody : codetree  =
                let
                    val n = mkLoad (~1, 0)                 (* the outer parameter *)
                    val f = mkEval (ioOpEntry, [n], true); (* early evaluation possible *)
                in
                    mkEnv
                      [
                        mkDec (1, f),
                        innerLambda
                      ]
                end;
                
                val outerLambda : codetree  =
                let
                    val level = 1;
                    val args  = 1;
                in
                    mkInlproc (outerBody, level, args, name)
                end;
            in
                outerLambda
            end;
            
            val runCall0Entry = makeRunCallTupled 0;
            val runCall2Entry = makeRunCallTupled 2;
            val runCall3Entry = makeRunCallTupled 3;
            val runCall4Entry = makeRunCallTupled 4;
            val runCall5Entry = makeRunCallTupled 5;
        in
            val () = enterRunCall ("run_call0", runCall0Entry, runCall0Type);
            val () = enterRunCall ("run_call1", runCall1Entry, runCall1Type);
            val () = enterRunCall ("run_call2", runCall2Entry, runCall2Type);
            val () = enterRunCall ("run_call3", runCall3Entry, runCall3Type);
            val () = enterRunCall ("run_call4", runCall4Entry, runCall4Type);
            val () = enterRunCall ("run_call5", runCall5Entry, runCall5Type);
        end;

(*****************************************************************************)
(*                  Bootstrapping functions (in structure Bootstrap)         *)
(*****************************************************************************)
        local
            val bootstrap = makeEmptyGlobal "Bootstrap";
        in
            val ()        = #enterStruct globalEnv ("Bootstrap", bootstrap);
            val (Env bootstrapEnv) = makeEnv (structSignat bootstrap);
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
            val () = addVal ("convStringName", "convString", String);
            val () = addVal ("convInt", intOfString, String ->> Int);
            val () = addVal ("convWord", wordOfString, String ->> Word);
        	(* Convert a string, recognising and converting the escape codes. *)
            val () = addVal ("convString", unescapeString, String ->> String);

        end;

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
            val eqVal  = mkSpecialFun "=" eqType Equal;
        in
            val () = enterGlobalValue ("=", eqVal);
        end;        

        local
            val neqType = let val a = makeEqTV () in a ** a ->> Bool end;
            val neqVal  = mkSpecialFun "<>" neqType NotEqual;
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
            val (Env polyMLEnv) = makeEnv (structSignat polyml);
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
            val (Env nameSpaceEnv) = makeEnv (structSignat nameSpace);
    
            (* Types for the basic values.  These are opaque. *)
            fun createType typeName =
            let
                val typeconstr =
                    makeFrozenTypeConstrs(typeName, [], makeFreeId(monotypeCode, false), 0, declInBasis);
            in
                #enterType nameSpaceEnv (typeName, typeconstr);
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
            val () = #enterType nameSpaceEnv ("nameSpace", recordType);
    
            val debugFields =
                List.map (fn (n,v) => mkLabelEntry("lookup" ^ n, String ->> Option v)) valTypes
            val debugRecord = mkLabelled(sortLabels debugFields, true);
            val debugType = (debugRecord ** (Unit ->> Bool)) ->> Unit
            val setDbtype = debugType ->> Unit
            
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

            val valueVal = valueVal
            val typeVal = typeVal
            val fixityVal = fixityVal
            val signatureVal = signatureVal
            val structureVal = structureVal
            val functorVal = functorVal
            
            val nameSpaceEnv = nameSpaceEnv
         end
         
        local
            open TYPETREE
            
            val fields =
                [
                    mkLabelEntry("file", String), mkLabelEntry("startLine", Int),
                    mkLabelEntry("startPosition", Int), mkLabelEntry("endLine", Int),
                    mkLabelEntry("endPosition", Int)
                ]
            val typeconstr =
                makeTypeAbbreviation("location", [], mkLabelled(sortLabels fields, true), declInBasis);
            val () = #enterType polyMLEnv ("location", typeconstr);
        in
            val Location = mkTypeConstruction ("location", typeconstr, [], declInBasis)
        end

(*****************************************************************************)
(*                  context type                                           *)
(*****************************************************************************)
        local
            (* Pretty print context information. *)
            open TYPETREE
            val typeconstr = makeDatatypeConstr("context", [], makeFreeId(monotypeCode, false), 0, declInBasis);
            val ContextType = mkTypeConstruction ("pretty", typeconstr, [], declInBasis)
            val () = #enterType polyMLEnv ("context", typeconstr);
 
            val constrs = (* Order is significant. *)
               [ ("ContextLocation", Location),
                 ("ContextParentStructure", String ** List ContextType),
                 ("ContextProperty", String ** String)];
            (* We rely on chooseConstrRepr giving us the same representation for the
               constructors as when context was compiled by the previous compiler. *)
            val numConstrs = List.length constrs
            val constructors =
                ListPair.map (fn ((s,t), code) =>
                    mkGconstr(s, t ->> ContextType, code, false, numConstrs, declInBasis))
                        (constrs, chooseConstrRepr constrs)
            val () = List.app (fn c => #enterVal polyMLEnv(valName c, c)) constructors
            (* Put these constructors onto the context type. *)
			val () = tcSetConstructors (typeconstr, constructors);
		in
            val Context = mkTypeConstruction ("context", typeconstr, [], declInBasis)
		end

(*****************************************************************************)
(*                  pretty datatype (for printing)                           *)
(*****************************************************************************)
        local
            open TYPETREE
            val typeconstr = makeDatatypeConstr("pretty", [], makeFreeId(monotypeCode, false), 0, declInBasis);
            val PrettyType = mkTypeConstruction ("pretty", typeconstr, [], declInBasis)
            val () = #enterType polyMLEnv ("pretty", typeconstr);
            val constrs = (* Order is significant. *)
               [ ("PrettyBlock", mkProductType[Int, Bool, List Context, List PrettyType]),
                 ("PrettyBreak", Int ** Int),
                 ("PrettyString", String)];
            (* We rely on chooseConstrRepr giving us the same representation for the
               constructors as when PrettyPrint was compiled by the previous compiler. *)
            val numConstrs = List.length constrs
            val constructors =
                ListPair.map (fn ((s,t), code) =>
                    mkGconstr(s, t ->> PrettyType, code, false, numConstrs, declInBasis))
                        (constrs, chooseConstrRepr constrs)
            val () = List.app (fn c => #enterVal polyMLEnv(valName c, c)) constructors
            (* Put these constructors onto the pretty print type. *)
			val () = tcSetConstructors (typeconstr, constructors);
		in
            val PrettyType = PrettyType
		end


(*****************************************************************************)
(*              Funny polymorphic functions (in structure PolyML)            *)
(*****************************************************************************)
        local
            val printType = let val a = makeTV () in a ->> a end;
            val printVal  = mkSpecialFun "print" printType Print;
        in
            val () = enterPolyMLVal ("print", printVal);
        end;

        local
            val printType =
                let val a = makeTV () in TYPETREE.mkProductType[a, nameSpaceType, PrettyType ->> Unit, Int] ->> Unit end;
            val printVal  = mkSpecialFun "printInNameSpace" printType PrintSpace;
        in
            val () = enterPolyMLVal ("printInNameSpace", printVal);
        end;

        local
            val makeStringType = let val a = makeTV () in a ->> String end;
            val makeStringVal  = mkSpecialFun "makestring" makeStringType MakeString;
        in
            val () = enterPolyMLVal ("makestring", makeStringVal);
        end;

        local
            val makeStringType = let val a = makeTV () in a ** nameSpaceType ->> String end;
            val makeStringVal  = mkSpecialFun "makestringInNameSpace" makeStringType MakeStringSpace;
        in
            val () = enterPolyMLVal ("makestringInNameSpace", makeStringVal);
        end;
 
        local
            (* addPrettyPrinter is the new function to install a pretty printer. *)
            val a = makeTV ();
            val b = makeTV ();
        
            val addPrettyType = (Int ->> b ->> a ->> PrettyType) ->> Unit;
            val addPrettyVal  = mkSpecialFun "addPrettyPrinter" addPrettyType AddPretty;
        in
            val () = enterPolyMLVal ("addPrettyPrinter", addPrettyVal);
        end;

        local
            (* Old install_pp function to install a pretty-printer. *)
            val a = makeTV ();
            val b = makeTV ();
        
            val printTupleType =
                TYPETREE.mkProductType
                 [
                   String ->> Unit,      (* addString *)
                   Int ** Bool ->> Unit, (* beginBracket *)
                   Int ** Int ->> Unit,  (* space *)
                   Unit ->> Unit         (* endBracket *)
                 ];
            val installPPType = (printTupleType ->> Int ->> b ->> a ->> Unit) ->> Unit;
            val installPPVal  = mkSpecialFun "install_pp" installPPType InstallPP;
        in
            val () = enterPolyMLVal ("install_pp", installPPVal);
        end;

        (* This goes in RunCall since it's only for the basis library. *)
        local
            val addOverloadType =
                let val a = makeTV () and b = makeTV () in (a ->> b) ->> String ->> Unit end;
            val addOverloadVal  = mkSpecialFun "addOverload" addOverloadType AddOverload;
        in
            val () = #enterVal runCallEnv ("addOverload", addOverloadVal);
        end;

(*****************************************************************************)
(*                  Bootstrap.Universal                                      *)
(*****************************************************************************)
        local
            (* This is used as one of the arguments to the compiler function. *)
            open TYPETREE
            val uniStruct = makeEmptyGlobal "Universal";
            val _ = #enterStruct bootstrapEnv ("Universal", uniStruct);
            val (Env uniStructEnv) = makeEnv (structSignat uniStruct);

            fun enterUniversal (name : string, entry : codetree, typ : types) : unit =
            let
                val value = mkGvar (name, typ, entry, declInBasis);
            in
                #enterVal uniStructEnv (name, value)
            end;

            (* type 'a tag *)
            val tagConstr = makeFrozenTypeConstrs("tag", [makeTV()], makeFreeId(polytypeCode, false), 0, declInBasis);
            val () = #enterType uniStructEnv ("tag", tagConstr);

            (* type universal *)
            val univConstr = makeFrozenTypeConstrs("universal", [], makeFreeId(monotypeCode, false), 0, declInBasis);
            val () = #enterType uniStructEnv ("universal", univConstr);

            fun Tag base = mkTypeConstruction ("tag", tagConstr, [base], declInBasis)
            val Universal = mkTypeConstruction ("universal", univConstr, [], declInBasis)

            (* val tagInject  : 'a tag -> 'a -> universal *)
            val injectType = let val a = makeTV() in Tag a ->> a ->> Universal end
            val () = enterUniversal ("tagInject", mkConst (toMachineWord Universal.tagInject), injectType)
            (* We don't actually need tagIs and tagProject since this is only used for
               the compiler.  Universal is redefined in the basis library. *)          
            val projectType = let val a = makeTV() in Tag a ->> Universal ->> a end
            val () = enterUniversal ("tagProject", mkConst (toMachineWord Universal.tagProject), projectType)
            val testType = let val a = makeTV() in Tag a ->> Universal ->> Bool end
            val () = enterUniversal ("tagIs", mkConst (toMachineWord Universal.tagIs), testType)
         in
            val Tag = Tag and Universal = Universal
        end

(*****************************************************************************)
(*                  Bootstrap.ExnMessage                                     *)
(*****************************************************************************)
(* This wraps PolyML.makestring to allow it to be used in General.exnMessage
   without capturing the environment at that point. *)
        local
            open TYPETREE
            val exnStruct = makeEmptyGlobal "ExnMessage";
            val _ = #enterStruct bootstrapEnv ("ExnMessage", exnStruct);
            val (Env exnStructEnv) = makeEnv (structSignat exnStruct);
        in
            val () = #enterVal exnStructEnv
                ("exnMessage", makeOverloaded("exnMessage", exnType ->> String, MakeString))
        end
		
(*****************************************************************************)
(*                  parseTree type                                           *)
(*****************************************************************************)
        local
            open TYPETREE
            (* Parsetree properties datatype. *)
			val propConstr = makeDatatypeConstr("ptProperties", [], makeFreeId(monotypeCode, false), 0, declInBasis);
            val () = #enterType polyMLEnv ("ptProperties", propConstr);
            val PtProperties = mkTypeConstruction ("ptProperties", propConstr, [], declInBasis)

            (* Parsetree type. *)
			val parseTreeConstr =
                makeTypeAbbreviation("parseTree", [], Location ** List PtProperties, declInBasis);    
            val ParseTree = mkTypeConstruction ("parseTree", parseTreeConstr, [], declInBasis)
            val () = #enterType polyMLEnv ("parseTree", parseTreeConstr);

            (* Representation of the type of a value. *)
			val typesConstr =
                makeFrozenTypeConstrs("typeExpression", [], makeFreeId(monotypeCode, false), 0, declInBasis);    
            val Types = mkTypeConstruction ("typeExpression", typesConstr, [], declInBasis)
            val () = #enterType polyMLEnv ("typeExpression", typesConstr);

            val constrs = (* Order is significant. *)
               [ ("PTdeclaredAt",       Location),
                 ("PTfirstChild",       Unit ->> ParseTree),
                 ("PTnextSibling",      Unit ->> ParseTree),
                 ("PTopenedAt",         Location),
                 ("PTparent",           Unit ->> ParseTree),
                 ("PTpreviousSibling",  Unit ->> ParseTree),
                 ("PTprint",            Int ->> PrettyType),
                 ("PTstructureAt",      Location),
                 ("PTtype",             Types)
                 ];
            (* We rely on chooseConstrRepr giving us the same representation for the
               constructors as when context was compiled by the previous compiler. *)
            val numConstrs = List.length constrs
            val constructors =
                ListPair.map (fn ((s,t), code) =>
                    mkGconstr(s, t ->> PtProperties, code, false, numConstrs, declInBasis))
                        (constrs, chooseConstrRepr constrs)
            val () = List.app (fn c => #enterVal polyMLEnv(valName c, c)) constructors
            (* Put these constructors onto the type. *)
			val () = tcSetConstructors (propConstr, constructors);

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
				    mkProductType[Option ParseTree, Option (Unit ->> execResult)];
        in
            val () = enterBootstrap ("use", mkConst (toMachineWord (useIntoEnv globalTable)), String ->> Unit)            
            val () = enterPolyMLVal("compiler", mkGvar ("compiler", compilerType, mkConst (toMachineWord compiler), declInBasis));
            val () = enterBootstrap("globalSpace", mkConst (toMachineWord(gEnvAsNameSpace globalTable)), nameSpaceType)
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
(*                  Funny functor PolyML.Run_exception0                      *)
(*****************************************************************************)
   (* "Run_exception0" and "Run_exception1" allow exceptions in the run-time
      system to be passed into ML. Run_exception0 is used for exceptions without
      arguments, and Run_exception1 for exceptions that have arguments. *)
   (* functor Run_exception0(val ex_iden: int end) : sig exception ex end *)
        
        local
            (* Argument signature. *)
            local
                (* make an anonymous, empty signature *)
                val sig0    : signatures = makeSignatures ("", inBasis);
                val (Env argEnv) = makeEnv sig0;
                val exIdenVal : values = makeFormalV ("ex_iden", Int, 0, declInBasis);
                val () = #enterVal argEnv ("ex_iden", exIdenVal);
            
            in
                val argSig0 = makeCopy ("", sig0, 0, 0, inBasis, fn _ => raise Subscript)
            end

            (* Result signature. *)
            local
                (* make an anonymous, empty signature *)
                val sig0    = makeSignatures ("", inBasis)
                val (Env resEnv) = makeEnv sig0;
                val exVal = makeFormalEx ("ex", TYPETREE.exnType, 0, declInBasis);
                val () = #enterVal resEnv ("ex", exVal);
            in
                val resSig0 = makeCopy ("", sig0, 0, 0, inBasis, fn _ => raise Subscript);
            end
                 
            (* The functor turns the value into an exception by returning the argument. *)
            val Run_exception0 = 
                makeFunctor
                (
                    "Run_exception0",
                    makeLocalStruct ("", argSig0, declInBasis),
                    resSig0,
                    makeGlobal (identityFunction "Run_exception0"),
                    inBasis
                );
        in
            val () = #enterFunct runCallEnv ("Run_exception0", Run_exception0);
        end;
   

(*****************************************************************************)
(*                  Funny functor RunCall.Run_exception1                     *)
(*****************************************************************************)
   (* functor Run_exception1(sig type exType; val ex_iden: int end) :
             sig exception ex of exType end *) 
        local
            (* Make a nullary type constructor (the type of the exception) *)
            val boundId = makeBoundId (monotypeCode(* Unused*), 0, false)
            val exTypeConstr = 
                makeFrozenTypeConstrs ("ex_type", [], boundId, 0, declInBasis);
            
            (* Argument signature. *)
            local
                (* make an anonymous, empty signature *)
                val sig1 = makeSignatures ("", inBasis);
                
                val (Env argEnv) = makeEnv sig1;
                
                val exIdenVal = makeFormalV ("ex_iden", Int, 0, declInBasis);
                
                val () = #enterType argEnv ("ex_type", exTypeConstr);
                val () = #enterVal  argEnv ("ex_iden", exIdenVal);
            in
                (* Contains 1 bound type *)
                val argSig1 =
                    makeCopy ("", sig1, 0, 1, inBasis, fn 0 => boundId | _ => raise Subscript);
            end
            
            (* Result signature. *)
            local
                (* make an anonymous, empty signature *)
                val sig1 = makeSignatures ("", inBasis);
                val (Env resEnv) = makeEnv sig1;
                
                (* get the actual type from the nullary type constructor *)
                val exType = mkTypeConstruction ("ex_type", exTypeConstr, [], declInBasis);
                val exVal  = makeFormalEx ("ex", exType ->> TYPETREE.exnType, 0, declInBasis);
                
                val () = #enterVal resEnv ("ex", exVal);
            in      
                (* 1 bound type inherited from argument sig *)
                val resSig1 = makeCopy ("", sig1, 0, 1, inBasis, fn 0 => boundId |  _ => raise Subscript);
            end
             
            (* The functor turns the value into an exception by returning the argument. *)
            val Run_exception1 = 
                makeFunctor
                    (
                    "Run_exception1",
                    makeLocalStruct ("", argSig1, declInBasis),
                    resSig1,
                    makeGlobal (identityFunction "Run_exception1"),
                    inBasis
                    );
        in
            val () = #enterFunct runCallEnv ("Run_exception1", Run_exception1);
        end;
   
(*****************************************************************************)
(*                  Bootstrap entries copied from DEBUG                *)
(*****************************************************************************)
        local
            open DEBUG;
            val debuggerType =
                TYPETREE.mkProductType[Int, valueVal, Int, String, String, nameSpaceType] ->> Unit

            val errorMessageProcType =
            let
                open TYPETREE
                val fields =
                [
                    mkLabelEntry("location", Location), mkLabelEntry("hard", Bool),
                    mkLabelEntry("message", PrettyType), mkLabelEntry("context", Option PrettyType)
                ]
            in
                mkLabelled(sortLabels fields, true) ->> Unit
            end
                
        in
            val () = applyList (fn (name, v, t) => enterBootstrap(name, mkConst v, t))
                [
                ("compilerVersion",        toMachineWord VERSION.compilerVersion, String),
                ("lineNumberTag",          toMachineWord lineNumberTag,          Tag (Unit->>Int)),
                ("offsetTag",              toMachineWord offsetTag,              Tag (Unit->>Int)),
                ("fileNameTag",            toMachineWord fileNameTag,            Tag String),
                ("maxInlineSizeTag",       toMachineWord maxInlineSizeTag,       Tag Int),
                ("assemblyCodeTag",        toMachineWord assemblyCodeTag,        Tag Bool),
                ("parsetreeTag",           toMachineWord parsetreeTag,           Tag Bool),
                ("codetreeTag",            toMachineWord codetreeTag,            Tag Bool),
                ("pstackTraceTag",         toMachineWord pstackTraceTag,         Tag Bool),
                ("codetreeAfterOptTag",    toMachineWord codetreeAfterOptTag,    Tag Bool),
                ("traceCompilerTag",       toMachineWord traceCompilerTag,       Tag Bool),
                ("inlineFunctorsTag",      toMachineWord inlineFunctorsTag,      Tag Bool),
                ("debugTag",               toMachineWord debugTag,        		 Tag Bool),
                ("profilingTag",           toMachineWord DEBUG.profilingTag,     Tag Int),
                ("timingTag",              toMachineWord DEBUG.timingTag,        Tag Bool),
                ("printDepthFunTag",       toMachineWord DEBUG.printDepthFunTag, Tag (Unit->>Int)),
                ("errorDepthTag",          toMachineWord DEBUG.errorDepthTag,    Tag Int),
                ("lineLengthTag",          toMachineWord DEBUG.lineLengthTag,    Tag Int),
                ("printEnvironTag",        toMachineWord VALUEOPS.printSpaceTag, Tag nameSpaceType),
                ("debuggerTag",            toMachineWord DEBUGGER.debuggerFunTag, Tag debuggerType),
                ("printOutputTag",         toMachineWord PRETTY.printOutputTag,  Tag (PrettyType->>Unit)) ,               
                ("compilerOutputTag",      toMachineWord PRETTY.compilerOutputTag, Tag (PrettyType->>Unit)),
                ("errorMessageProcTag",    toMachineWord LEX.errorMessageProcTag, Tag errorMessageProcType),
                ("rootTreeTag",            toMachineWord EXPORTTREE.rootTreeTag, Tag (Option(Unit->>ParseTree)))
                ]
        end;
 
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
            
            fun getValue (Value{access = Global code, class = SimpleValue, ...}) = evalue code
            |   getValue _ = raise Fail "Not a global value"
        in
            (* Add these to the PolyML.NameSpace structure. *)
            val () = applyList (fn (name, v, t) =>
                                #enterVal nameSpaceEnv (name, mkGvar (name, t, mkConst v, declInBasis)))
                [
                ("displayFix",             toMachineWord displayFix, String ** fixityVal ->> PrettyType),
                ("displaySig",             toMachineWord displaySignatures,
                    mkProductType[signatureVal, Int, nameSpaceType] ->> PrettyType),
                ("displayStruct",             toMachineWord displayStructures,
                    mkProductType[structureVal, Int, nameSpaceType] ->> PrettyType),
                ("displayFunct",             toMachineWord displayFunctors,
                    mkProductType[functorVal, Int, nameSpaceType] ->> PrettyType),
                ("displayVal",             toMachineWord displayValues,
                    mkProductType[valueVal, Int, nameSpaceType] ->> PrettyType),
                ("displayType",             toMachineWord displayTypeConstrs,
                    mkProductType[typeVal, Int] ->> PrettyType),
                (* displayTypeExpression doesn't really belong here since it's used
                   as part of the parse tree rather the name space. *)
                ("displayTypeExpression",    toMachineWord TYPETREE.display,
                    mkProductType[Types, Int] ->> PrettyType)
                ]
            (* Put this in Bootstrap, at least for the moment.
               Used to print values in the debugger without the "val", "=" and the type. *)
           val () = applyList (fn (name, v, t) => enterBootstrap(name, mkConst v, t))
                [("printValue",             toMachineWord printValues,
                    mkProductType[valueVal, Int, nameSpaceType] ->> PrettyType),
                 (* This is used to get the actual value out of a global "value".
                    It's currently used only in the debugger to get the exception
                    packet out of a global exception value. *)
                 ("getValue",               toMachineWord getValue,
                    valueVal ->> mkTypeVar (generalisable, false,  false, false))]
        end;

    in
        ()
    end (* initGlobalEnv *);
end;
