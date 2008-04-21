(*
	Copyright (c) 2000
		Cambridge University Technical Services Limited

    Updated David C.J. Matthews 2008

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

(*****************************************************************************)
(*                  TYPETREE                                                 *)
(*****************************************************************************)
structure TYPETREE :
sig
  type types;
  type typeConstrs;

  val mkTypeVar:          int * bool * bool * bool -> types;
  val mkTypeConstruction: string * typeConstrs * types list -> types;
  val mkProductType:      types list -> types;
  val mkFunctionType:     types * types -> types;
  val mkOverloadSet:	  typeConstrs list -> types;
  val mkLabelled:         {name: string, typeof: types } list * bool -> types;
  val mkLabelEntry:       string * types -> {name: string, typeof: types };

  val sortLabels:         {name: string, typeof: types } list * (string -> unit) ->
  								{name: string, typeof: types } list;
   
  val boolType:   types;
  val intType:    types;
  val stringType: types;
  val unitType:   types;
  val charType:   types;
  val wordType:   types;
  val exnType:    types;
  
  type prettyPrinter
  val displayTypeConstrs: typeConstrs * int * prettyPrinter * bool -> unit
end;

(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS : 
sig

  (* Structures *)
  type structVals;
  type signatures;
  type valAccess;
  type codetree;
  type values;
  type typeId;

  val undefinedStruct:   structVals;
  val structSignat:      structVals -> signatures;
  
  val makeEmptyGlobal:   string -> structVals;
  val makeLocalStruct:   string * signatures -> structVals;

  (* Functors *)
  type functors;
  val makeFunctor: string * structVals * signatures * valAccess -> functors;

  (* Signatures *)
  type univTable;
  val sigTab:        signatures -> univTable;
  val makeSignatures: string -> signatures;
  val makeCopy: string * signatures * int * int -> signatures;

  val makeFreeId:  unit -> typeId;
  val makeBoundId: int  -> typeId;
  
  (* Types *)
  type types;
  type typeConstrs;

  val emptyType: types;
  val tcSetConstructors: typeConstrs * values list -> unit;
  val tcTypeVars:     typeConstrs -> types list
  val makeTypeConstrs:
  	string * types list * types * typeId * bool * int -> typeConstrs;
  val makeFrozenTypeConstrs:
  	string * types list * types * typeId * bool * int -> typeConstrs;

  val generalisable: int;
  
  val boolType:   typeConstrs;
  val intType:    typeConstrs;
  val charType:   typeConstrs;
  val stringType: typeConstrs;
  val realType:   typeConstrs;
  val refType:    typeConstrs;
  val unitType:   typeConstrs;
  val exnType:    typeConstrs;
  val wordType:   typeConstrs;
  val listType:   typeConstrs;

  (* Access to values, structures etc. *)
  val makeGlobal:   codetree -> valAccess;

  (* Values. *)
  
  datatype typeDependent =
    Print
  | PrintSpace
  | MakeString
  | MakeStringSpace
  | InstallPP
  | Equal
  | NotEqual
  | AddOverload
  | TypeDep;
  
  val makeFormalV: string * types * int -> values;  
  val makeFormalEx: string * types * int -> values;  
  val makeOverloaded: string * types * typeDependent -> values;
  
  type fixStatus;
  datatype env = Env of
    {
      lookupVal:    string -> values option,
      lookupType:   string -> typeConstrs option,
      lookupFix:    string -> fixStatus option,
      lookupStruct: string -> structVals option,
      lookupSig:    string -> signatures option,
      lookupFunct:  string -> functors option,
      enterVal:     string * values      -> unit,
      enterType:    string * typeConstrs -> unit,
      enterFix:     string * fixStatus   -> unit,
      enterStruct:  string * structVals  -> unit,
      enterSig:     string * signatures  -> unit,
      enterFunct:   string * functors    -> unit
    };

  val makeEnv: signatures -> env;

  type 'a tag;
  
  val valueVar:      values      tag;
  val typeConstrVar: typeConstrs tag;
  val fixVar:        fixStatus   tag;
  val structVar:     structVals  tag;
  val signatureVar:  signatures  tag;
  val functorVar:    functors    tag;
end;


(*****************************************************************************)
(*                  VALUEOPS                                                 *)
(*****************************************************************************)
structure VALUEOPS :
sig
  type codetree
  type types
  type values
  type representations
  type prettyPrinter
  type structVals
  type functors
  type signatures
  type fixStatus
  type typeConstrs
  type nameSpace;
  
  (* Construction functions. *)
  val mkGvar:    string * types * codetree -> values
  val mkGex:     string * types * codetree -> values
  val mkGconstr: string * types * codetree * bool -> values
 
  (* Standard values *)
  val nilConstructor:  values;
  val consConstructor: values;
   
  val RefForm:   representations;
  val BoxedForm: representations;
  val EnumForm:  int -> representations;

  val createNullaryConstructor: representations * string -> codetree
  val createUnaryConstructor: representations * string -> codetree
  
  val displayFixStatus:  fixStatus  * int * prettyPrinter -> unit
  (* nameSpace arg is used to get fixity and for exception packets. bool arg is ture if we
     should print type with the structure name. *)
  val displaySignatures: signatures * int * prettyPrinter * nameSpace * bool -> unit
  val displayStructures: structVals * int * prettyPrinter * nameSpace * bool -> unit
  val displayFunctors:   functors   * int * prettyPrinter * nameSpace * bool -> unit
  val displayValues:     values * int * prettyPrinter * nameSpace * bool -> unit
  val printValues: values * int * prettyPrinter * nameSpace -> unit

  val printSpaceTag: nameSpace Universal.tag

end;

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
end;

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

    val compiler :
        nameSpace * (unit->char option) * Universal.universal list -> unit ->
       { fixes: (string * fixStatus) list, values: (string * values) list,
         structures: (string * structVals) list, signatures: (string * signatures) list,
         functors: (string * functors) list, types: (string* typeConstrs) list };
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
  val errorMessageProcTag: (string * bool * int -> unit) Universal.tag
  val compilerOutputTag: (string->unit) Universal.tag
  val lineNumberTag: (unit->int) Universal.tag
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
  val ml90Tag				  : bool Universal.tag;
  val debugTag				  : bool Universal.tag;

  val printStringTag : (string->unit) Universal.tag;
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
structure PRETTYPRINTER :
sig
  type prettyPrinter 
  val prettyPrint : int * (string -> unit) -> prettyPrinter; 
  val uglyPrint   : (string -> unit) -> prettyPrinter; 
end;

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
  VALUEOPS.prettyPrinter
= TYPETREE.prettyPrinter
= PRETTYPRINTER.prettyPrinter

sharing type
  MAKE.nameSpace
= VALUEOPS.nameSpace

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
            val Char	= charType;
            val Word   = wordType;
            
            val mkTypeConstruction = mkTypeConstruction;
            val mkTypeVar = mkTypeVar;
        end;
   
        (* List of something *)
        fun List (base : types) : types =
            mkTypeConstruction ("list", listType, [base]);

        (* ref something *)
        fun Ref (base : types) : types  =
            mkTypeConstruction ("ref", refType, [base]);
        
        (* option something *)
        val optionType =
            makeTypeConstrs("option", [makeTV()], emptyType, makeFreeId (), true, 0);
        
        fun Option (base : types) : types  =
            mkTypeConstruction ("option", optionType, [base]);
        
        
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
                    createNullaryConstructor(EnumForm 0, "false"), true);
            val trueCons  =
                mkGconstr ("true",  Bool,
                    createNullaryConstructor(EnumForm 1, "true"), true);
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
            val chrVal  = mkGvar ("chr", chrType, chrCode);
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
            val alphaOption   = mkTypeConstruction ("option", optionType, optionTypeVars);
            val someType      = TYPETREE.mkFunctionType (alpha, alphaOption);
            (* These two representations are built into the RTS. *)
            val NoneForm = EnumForm 0;
            val SomeForm = BoxedForm;
            val noneConstructor  =
                mkGconstr ("NONE", alphaOption,
                    createNullaryConstructor(NoneForm, "NONE"),  true);
            val someConstructor =
                mkGconstr ("SOME",  someType,
                    createUnaryConstructor(SomeForm, "SOME"), false);
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
                        createUnaryConstructor(RefForm, "ref"), false)
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
            val plingVal  = mkGvar ("!", plingType, plingCode);
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
   vector to be an equality type and to use structure equality.  We can't
   add an overload for "=" as we do with arrays because that causes the
   type to have a ref-style equality i.e. 'a vector would permit equality
   even if 'a did not. *)
        val vectorType =
            makeFrozenTypeConstrs("vector", [makeTV()], emptyType, makeFreeId (), true, 0);
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
            val value = mkGvar (name, typ, entry);
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
            val value = mkGvar (name, typ, entry);
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
                val typeconstr = makeTypeConstrs(typeName, [], emptyType, makeFreeId (), false, 0);
            in
                #enterType nameSpaceEnv (typeName, typeconstr);
                mkTypeConstruction (typeName, typeconstr, [])
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
                makeTypeConstrs("nameSpace", [],
                     mkLabelled(sortLabels(fields, fn _ => ()), true), makeFreeId (), false, 0);
            val () = #enterType nameSpaceEnv ("nameSpace", recordType);
    
            val debugFields =
                List.map (fn (n,v) => mkLabelEntry("lookup" ^ n, String ->> Option v)) valTypes
            val debugRecord = mkLabelled(sortLabels(debugFields, fn _ => ()), true);
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
            val nameSpaceType = mkTypeConstruction ("nameSpace", recordType, [])
            val execResult = mkLabelled(sortLabels(resultFields, fn _ => ()), true)

            val valueVal = valueVal
            val typeVal = typeVal
            val fixityVal = fixityVal
            val signatureVal = signatureVal
            val structureVal = structureVal
            val functorVal = functorVal
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
                let val a = makeTV () in TYPETREE.mkProductType[a, nameSpaceType, String ->> Unit, Int] ->> Unit end;
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
                val value = mkGvar (name, typ, entry);
            in
                #enterVal uniStructEnv (name, value)
            end;

            (* type 'a tag *)
            val tagConstr = makeTypeConstrs("tag", [makeTV()], emptyType, makeFreeId (), false, 0);
            val () = #enterType uniStructEnv ("tag", tagConstr);

            (* type universal *)
            val univConstr = makeTypeConstrs("universal", [], emptyType, makeFreeId (), false, 0);
            val () = #enterType uniStructEnv ("universal", univConstr);

            fun Tag base = mkTypeConstruction ("tag", tagConstr, [base])
            val Universal = mkTypeConstruction ("universal", univConstr, [])

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
(*        PolyML.compiler etc                                                *)
(*****************************************************************************)

        local
            open TYPETREE
 
            val compilerType : types =
                mkProductType[nameSpaceType, Unit ->> Option Char, List Universal] ->> Unit ->> execResult;
        in
            val () = enterBootstrap ("use", mkConst (toMachineWord (useIntoEnv globalTable)), String ->> Unit)            
            val () = enterPolyMLVal("compiler", mkGvar ("compiler", compilerType, mkConst (toMachineWord compiler)));
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
                val sig0    : signatures = makeSignatures "";
                val (Env argEnv) = makeEnv sig0;
                val exIdenVal : values = makeFormalV ("ex_iden", Int, 0);
                val () = #enterVal argEnv ("ex_iden", exIdenVal);
            
            in
                val argSig0 = makeCopy ("", sig0, 0, 0)
            end

            (* Result signature. *)
            local
                (* make an anonymous, empty signature *)
                val sig0    = makeSignatures ""
                val (Env resEnv) = makeEnv sig0;
                val exType = emptyType;
                val exVal = makeFormalEx ("ex", exType, 0);
                val () = #enterVal resEnv ("ex", exVal);
            in
                val resSig0 = makeCopy ("", sig0, 0, 0);
            end
                 
            (* The functor turns the value into an exception by returning the argument. *)
            val Run_exception0 = 
                makeFunctor
                (
                    "Run_exception0",
                    makeLocalStruct ("", argSig0),
                    resSig0,
                    makeGlobal (identityFunction "Run_exception0")
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
            val exTypeConstr = 
                makeFrozenTypeConstrs ("ex_type", [], emptyType, makeBoundId 0, false, 0);
            
            (* Argument signature. *)
            local
                (* make an anonymous, empty signature *)
                val sig1 = makeSignatures "";
                
                val (Env argEnv) = makeEnv sig1;
                
                val exIdenVal = makeFormalV ("ex_iden", Int, 0);
                
                val () = #enterType argEnv ("ex_type", exTypeConstr);
                val () = #enterVal  argEnv ("ex_iden", exIdenVal);
            in
                (* Contains 1 bound type *)
                val argSig1 = makeCopy ("", sig1, 0, 1);
            end
            
            (* Result signature. *)
            local
                (* make an anonymous, empty signature *)
                val sig1 = makeSignatures "";
                val (Env resEnv) = makeEnv sig1;
                
                (* get the actual type from the nullary type constructor *)
                val exType = mkTypeConstruction ("ex_type", exTypeConstr, []);
                val exVal  = makeFormalEx ("ex", exType, 0);
                
                val () = #enterVal resEnv ("ex", exVal);
            in      
                (* 1 bound type inherited from argument sig *)
                val resSig1 = makeCopy ("", sig1, 0, 1);
            end
             
            (* The functor turns the value into an exception by returning the argument. *)
            val Run_exception1 = 
                makeFunctor
                    (
                    "Run_exception1",
                    makeLocalStruct ("", argSig1),
                    resSig1,
                    makeGlobal (identityFunction "Run_exception1")
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
        in
            val () = applyList (fn (name, v, t) => enterBootstrap(name, mkConst v, t))
                [
                ("compilerVersion",        toMachineWord VERSION.compilerVersion, String),

                ("errorMessageProcTag",    toMachineWord errorMessageProcTag,
                     Tag (TYPETREE.mkProductType[String, Bool, Int] ->> Unit)),
                ("compilerOutputTag", toMachineWord compilerOutputTag,  Tag (String->>Unit)),
                ("lineNumberTag", toMachineWord lineNumberTag,  Tag (Unit->>Int)),
                ("fileNameTag", toMachineWord fileNameTag,  Tag String),
                ("maxInlineSizeTag",       toMachineWord maxInlineSizeTag,       Tag Int),
                ("assemblyCodeTag",        toMachineWord assemblyCodeTag,        Tag Bool),
                ("parsetreeTag",           toMachineWord parsetreeTag,           Tag Bool),
                ("codetreeTag",            toMachineWord codetreeTag,            Tag Bool),
                ("pstackTraceTag",         toMachineWord pstackTraceTag,         Tag Bool),
                ("codetreeAfterOptTag",    toMachineWord codetreeAfterOptTag,    Tag Bool),
                ("traceCompilerTag",       toMachineWord traceCompilerTag,       Tag Bool),
                ("inlineFunctorsTag",      toMachineWord inlineFunctorsTag,      Tag Bool),
                ("ml90Tag",        		   toMachineWord ml90Tag,        		 Tag Bool),
                ("debugTag",               toMachineWord debugTag,        		 Tag Bool),
                ("profilingTag",           toMachineWord DEBUG.profilingTag,     Tag Int),
                ("timingTag",              toMachineWord DEBUG.timingTag,        Tag Bool),
                ("printDepthFunTag",       toMachineWord DEBUG.printDepthFunTag, Tag (Unit->>Int)),
                ("errorDepthTag",          toMachineWord DEBUG.errorDepthTag,    Tag Int),
                ("lineLengthTag",          toMachineWord DEBUG.lineLengthTag,    Tag Int),
        	    ("printStringTag",         toMachineWord DEBUG.printStringTag,   Tag (String->>Unit)),
                ("printEnvironTag",        toMachineWord VALUEOPS.printSpaceTag, Tag nameSpaceType),
                ("debuggerTag",            toMachineWord DEBUGGER.debuggerFunTag, Tag debuggerType)
                ]
        end;
 
(*****************************************************************************)
(*                  Bootstrap entries for printing                           *)
(*****************************************************************************)
        local
            open TYPETREE PRETTYPRINTER
            fun displayFix(f: fixStatus, stream: string->unit) =
                displayFixStatus(f, 999 (* Actually unused. *), prettyPrint (77, stream))

            and displaySig(s: signatures, depth: int, space: nameSpace, withStruct: bool, stream: string->unit) =
                displaySignatures(s, depth, prettyPrint (77, stream), space, withStruct)

            and displayStruct(s: structVals, depth: int, space: nameSpace, withStruct: bool, stream: string->unit) =
                displayStructures(s, depth, prettyPrint (77, stream), space, withStruct)

            and displayFunct(f: functors, depth: int, space: nameSpace, withStruct: bool, stream: string->unit) =
                displayFunctors(f, depth, prettyPrint (77, stream), space, withStruct)

            and displayVal(v: values, depth: int, space: nameSpace, withStruct: bool, stream: string->unit) =
                displayValues(v, depth, prettyPrint (77, stream), space, withStruct)

            and displayType(t: typeConstrs, depth: int, withStruct: bool, stream: string->unit) =
                displayTypeConstrs(t, depth, prettyPrint (77, stream), withStruct)

            (* Used to print values in the debugger.  Use uglyPrint here to keep it
               simple and on one line. *)
            and printVal(v: values, depth: int, space: nameSpace, stream: string->unit) =
                printValues(v, depth, uglyPrint stream, space)
        in
            val () = applyList (fn (name, v, t) => enterBootstrap(name, mkConst v, t))
                [
                ("displayFix",             toMachineWord displayFix, fixityVal ** (String ->> Unit) ->>Unit),
                ("displaySig",             toMachineWord displaySig,
                    mkProductType[signatureVal, Int, nameSpaceType, Bool, (String ->> Unit)] ->> Unit),
                ("displayStruct",             toMachineWord displayStruct,
                    mkProductType[structureVal, Int, nameSpaceType, Bool, (String ->> Unit)] ->> Unit),
                ("displayFunct",             toMachineWord displayFunct,
                    mkProductType[functorVal, Int, nameSpaceType, Bool, (String ->> Unit)] ->> Unit),
                ("displayVal",             toMachineWord displayVal,
                    mkProductType[valueVal, Int, nameSpaceType, Bool, (String ->> Unit)] ->> Unit),
                ("printVal",             toMachineWord printVal,
                    mkProductType[valueVal, Int, nameSpaceType, (String ->> Unit)] ->> Unit),
                ("displayType",             toMachineWord displayType,
                    mkProductType[typeVal, Int, Bool, (String ->> Unit)] ->> Unit)
                ]
        end;

    in
        ()
    end (* initGlobalEnv *);
end;
