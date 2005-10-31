(*
	Copyright (c) 2000
		Cambridge University Technical Services Limited

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

(* preliminary version *)

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
   
  val boolType:   types;
  val intType:    types;
  val stringType: types;
  val unitType:   types;
  val charType:   types;
end;

(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS : 
sig
  type word
  
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
  | MakeString
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
      lookupVal:    string -> values,
      lookupType:   string -> typeConstrs,
      lookupFix:    string -> fixStatus,
      lookupStruct: string -> structVals,
      lookupSig:    string -> signatures,
      lookupFunct:  string -> functors,
      lookupTvars:  string -> types,
      enterVal:     string * values      -> unit,
      enterType:    string * typeConstrs -> unit,
      enterFix:     string * fixStatus   -> unit,
      enterStruct:  string * structVals  -> unit,
      enterSig:     string * signatures  -> unit,
      enterFunct:   string * functors    -> unit
    };

  val makeEnv: signatures -> env;
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

end;

(*****************************************************************************)
(*                  CODETREE                                                 *)
(*****************************************************************************)
structure CODETREE :
sig
  type word
  type codetree
     
  val CodeNil:          codetree;
  val CodeZero:         codetree;
  val mkEnv:            codetree list -> codetree;
  val mkDec:            int * codetree -> codetree;
  val mkLoad:           int * int -> codetree;
  val mkInd:            int * codetree -> codetree;
  val mkConst:          word -> codetree;
  val mkInlproc:        codetree * int * int * string -> codetree;
  val mkIf:             codetree * codetree * codetree -> codetree;
  val mkEval:           codetree * codetree list * bool -> codetree;
  val identityFunction: string -> codetree;
end;

(*****************************************************************************)
(*                  MAKE                                                     *)
(*****************************************************************************)
structure MAKE :
sig
  type dbEnv
  type univTable
  type values
  
  val makeDbEnv    : univTable -> dbEnv
  val makeIntoEnv  : dbEnv -> string -> unit
  val useIntoEnv   : dbEnv -> string -> unit
  val dependsInEnv : dbEnv -> string -> unit
  val dependenciesInEnv : dbEnv -> string -> string list
  val runCompiler  : dbEnv -> 
       (unit -> string) * (string -> unit) -> unit -> unit
       
  val shellProc   : (dbEnv * string ref * string ref) -> 
    {
     shell:      unit -> unit,    (* The command processor *)
	 debugger:	 (string -> values)*(unit->bool) -> unit,
     startLog:   string -> unit,  (* Starts logging commands to a file *)
     restartLog: string -> unit,  (* Appends logging commands to a file *)
     stopLog:    unit -> unit,    (* Stops logging *)
     writeLog:   string -> unit,  (* Writes a string to the log file *)
     logName:    unit -> string,  (* Get the log name *)
     isLogging:  unit -> bool     (* true if logging, otherwise false *)
    }
    
  val forgetMakeInfo  : dbEnv -> string -> unit;
  val forgetSignature : dbEnv -> string -> unit;
  val forgetStructure : dbEnv -> string -> unit;
  val forgetFunctor   : dbEnv -> string -> unit;
  val forgetValue     : dbEnv -> string -> unit;
  val forgetType      : dbEnv -> string -> unit;
  val forgetFixity    : dbEnv -> string -> unit;
  
  val signatureNames : dbEnv -> unit -> string list;
  val structureNames : dbEnv -> unit -> string list;
  val functorNames   : dbEnv -> unit -> string list;
  val valueNames     : dbEnv -> unit -> string list;
  val typeNames      : dbEnv -> unit -> string list;
  val fixityNames    : dbEnv -> unit -> string list;
end;

(*****************************************************************************)
(*                  ADDRESS                                                  *)
(*****************************************************************************)
structure ADDRESS :
sig
  type word
  val toWord : 'a -> word;
end;

(*****************************************************************************)
(*                  RUNCALL                                                  *)
(*****************************************************************************)
structure RUNCALL :
sig
  val run_call1 : int -> 'a -> 'b;
end;

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
  type 'a dbRef
  val profiling  : int dbRef;
  val timing     : bool dbRef;
  val printDepth : int dbRef;
  val errorDepth : int dbRef;
  
  val assemblyCode        : bool ref;
  val parsetree           : bool ref;
  val codetree            : bool ref;
  val pstackTrace         : bool ref;
  val codetreeAfterOpt    : bool ref;
  val traceCompiler       : bool ref;
  val inlineFunctors      : bool ref;
  val maxInlineSize       : int ref;
  val useRCS              : bool ref;
  val suffixes            : string list ref;
  val ml90				  : bool ref;
  val debug				  : bool ref;

  val printInAlphabeticalOrder : bool ref;
  val printTypesWithStructureName : bool ref;
  
  val prompt1 : string ref;
  val prompt2 : string ref;

  val printString : (string->unit) ref;
  val inputChar : (unit->char option) ref;
  val nextIsNewline : (unit->bool) ref;
  val flushInput : (unit->unit) ref;
end;

(*****************************************************************************)
(*                  CREATEDATABASE                                           *)
(*****************************************************************************)
structure CREATEDATABASE :
sig
  type 'a dbRef
  val dbRefGet: 'a dbRef -> 'a;
  val dbRefPut: 'a dbRef * 'a -> unit;

  type 'a dbList
  val dbListCreate : (unit -> '_a) * '_a list -> '_a dbList
  val dbListToList :  'a dbList -> 'a list

  val createDatabase : string * (unit -> unit) * (unit -> bool) -> unit
end;

(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
sig
  type 'a iter
  val intOfString  : string -> int
  val unescapeString : string -> string
  val mapIterator : ('a -> 'b) -> 'a iter -> 'b list
  val upto : int -> int -> int iter
end;

(*****************************************************************************)
(*                  DEBUGGER                                                 *)
(*****************************************************************************)
structure DEBUGGER :
sig
	val singleStep: unit -> unit
	val stepOver: unit -> unit
	val stepOut: unit -> unit
	val addLineBreak: string * int -> unit
	val clearLineBreak: string * int -> unit
	val addFnBreak: string -> unit
	val clearFnBreak: string -> unit
	val continue: unit -> unit
	val upStack: unit -> unit
	val downStack: unit -> unit
	val dumpStack: unit -> unit
	val printVars: unit -> unit
	val stackTrace: unit -> unit
	val traceFunctions: bool -> unit
end;

(*****************************************************************************)
(*                  INITIALISE sharing constraints                           *)
(*****************************************************************************)

sharing type
  CREATEDATABASE.dbRef
= DEBUG.dbRef

sharing type
  ADDRESS.word
= CODETREE.word
    
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
  
sharing type
  STRUCTVALS.univTable
= MAKE.univTable 
  
) : 

(*****************************************************************************)
(*                  INITIALISE export signature                              *)
(*****************************************************************************)
sig
  type dbEnv
  val initGlobalEnv : (unit -> unit) * (unit -> bool) * 
                      string ref * string ref -> 
                         dbEnv * (unit -> unit)

  (* called to initialise a new (primary or secondary) process group *)
  val initialFn : (bool -> unit) ref;
  
  (* called at start of each session *)
  val startupFn : (unit -> unit) ref;
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
   open RUNCALL;
   open CREATEDATABASE;
   open MAKE;
   open MISC;
   open RuntimeCalls; (* for POLY_SYS calls *)

(*****************************************************************************)
(*                  Untility functions                                       *)
(*****************************************************************************)
  fun applyList f []       = ()
    | applyList f (h :: t) = (f h : unit; applyList f t);

(*****************************************************************************)
(*                  version strings                                          *)
(*****************************************************************************)
  (* The initialisation function was used in AHL's version to initialise
     their licence manager.  I don't think there's any use for it now.
	 DCJM June 2000. *)
  fun dummyInitialFn (b : bool) = ();
 
  fun dummyStartupFn () =
    TextIO.output (TextIO.stdOut, "Poly/ML 4.2.0 Release\n");
 
  val initialFn : (bool -> unit) ref = ref dummyInitialFn;
  val startupFn : (unit -> unit) ref = ref dummyStartupFn;

(*****************************************************************************)
(*                  initGlobalEnv                                            *)
(*****************************************************************************)
fun initGlobalEnv
     (rootShell    : unit -> unit,
      systemCommit : unit -> bool, 
      prompt1      : string ref,
      prompt2      : string ref) =
let
  local
     (* Create the global (top-level) environment *)
     val globalUnivTable = structSignat (makeEmptyGlobal "global");
  in
     (* create a functional view of it *)
     val (Env globalEnv) = makeEnv globalUnivTable;
   
     (* globalTable is like globalUnivTable except that it also
        implements automatic child-database inheritance *)
     val globalTable : dbEnv = makeDbEnv (sigTab globalUnivTable);
  end;

   
(*****************************************************************************)
(*                  Utilities                                                *)
(*****************************************************************************)
   (* shouldn't these be imported from somewhere? *)
   fun ioOp (x: int) : 'a = run_call1 POLY_SYS_io_operation x;
   val intZero    = mkConst (toWord 0);
   val realZero   = mkConst (toWord 0.0);

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
   val U : unit = enterGlobalType ("unit", unitType);

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
      val U : unit = enterGlobalType  ("bool",  boolType);
      val U : unit = enterGlobalValue ("true",  trueCons);
      val U : unit = enterGlobalValue ("false", falseCons);

     (* Put these constructors onto the boolean type. *)
     val U : unit =
       tcSetConstructors (boolType, [trueCons, falseCons]);
   end;


(*****************************************************************************)
(*                  int                                                      *)
(*****************************************************************************)
   val U : unit = enterGlobalType ("int", intType);
   
(*****************************************************************************)
(*                  char                                                     *)
(*****************************************************************************)
   val U : unit = enterGlobalType ("char", charType);
   
(*****************************************************************************)
(*                  string                                                   *)
(*****************************************************************************)
   val U : unit = enterGlobalType ("string", stringType);

   (* chr - define it as an identity function for now. It is redefined in
      the prelude to check that the value is a valid character. *)
   local
     val chrCode = identityFunction "chr";
     val chrType = Int ->> String;
     val chrVal  = mkGvar ("chr", chrType, chrCode);
   in
     val U : unit = enterGlobalValue ("chr", chrVal);
   end;        
    
(*****************************************************************************)
(*                  real                                                     *)
(*****************************************************************************)
   val U : unit = enterGlobalType ("real", realType);

(*****************************************************************************)
(*                  'a list                                                  *)
(*****************************************************************************)
   val U : unit = enterGlobalType  ("list", listType);
   val U : unit = enterGlobalValue ("::",   consConstructor);
   val U : unit = enterGlobalValue ("nil",  nilConstructor);

   (* Put these constructors onto the list type. *)
   val U : unit = tcSetConstructors (listType, [consConstructor, nilConstructor]);

(*****************************************************************************)
(*                  'a option                                                  *)
(*****************************************************************************)
  (* Type option does not need to be built in but because its representation is
     built into the RTS it's easier and safer to do it this way. *)
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
   val U : unit = enterGlobalType  ("option", optionType);
   val U : unit = enterGlobalValue ("SOME",   someConstructor);
   val U : unit = enterGlobalValue ("NONE",   noneConstructor);
   (* Put these constructors onto the option type.  N.B.  as with all
      value constructors the order is significant.  Shorter names come
	  first and if the length is the same the order is alphabetical. *)
   val U : unit = tcSetConstructors (optionType, [noneConstructor, someConstructor]);
  end;


(*****************************************************************************)
(*                  ref                                                      *)
(*****************************************************************************)
   local
     val refCons =
       let (* an imperative type variable *)
         val a : types = makeImpTV ();
       in
         mkGconstr ("ref", a ->> Ref a,
		 	createUnaryConstructor(RefForm, "ref"), false)
       end
   in
     val U : unit = enterGlobalType  ("ref", refType);
     val U : unit = enterGlobalValue ("ref", refCons);

     (* Put this constructor onto the ref type. *)
     val U : unit = tcSetConstructors (refType, [refCons]);
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
     val U : unit = enterGlobalValue ("!", plingVal);
   end;        

(*****************************************************************************)
(*                  exn                                                      *)
(*****************************************************************************)
   val U : unit = enterGlobalType ("exn", exnType);

(*****************************************************************************)
(*                  word                                                      *)
(*****************************************************************************)
   val U : unit = enterGlobalType ("word", wordType);

(*****************************************************************************)
(*                  'a vector                                                *)
(*****************************************************************************)
(* The only reason we have vector here is to get equality right.  We need
   vector to be an equality type and to use structure equality.  We can't
   add an overload for "=" as we do with arrays because that causes the
   type to have a ref-style equality i.e. 'a vector would permit equality
   even if 'a did not. *)
  val vectorType =
     makeTypeConstrs("vector", [makeTV()], emptyType, makeFreeId (), true, 0);
   val U : unit = enterGlobalType  ("vector", vectorType);

(*****************************************************************************)
(*                  System functions (in structure PolyML)                   *)
(*****************************************************************************)
   local
      val PolyML = makeEmptyGlobal "PolyML";
    in
      val U : unit        = #enterStruct globalEnv ("PolyML", PolyML);
      val (Env PolyMLEnv) = makeEnv (structSignat PolyML);
    end;
   
    fun enterPolyML (name : string, entry : codetree, typ : types) : unit =
    let
      val value = mkGvar (name, typ, entry);
    in
      #enterVal PolyMLEnv (name, value)
    end;

(*****************************************************************************)
(*                  PolyML.dbList etc                                        *)
(*****************************************************************************)
  
  local
    val dbListType : typeConstrs = (* Not an equality type *)
      makeTypeConstrs ("dbList", [makeTV ()], emptyType,
	  					makeFreeId (), false, 0);
   
    fun DbList (base : types) : types =
      mkTypeConstruction ("dbList", dbListType, [base]);

    local (* an imperative type variable *)
      val a : types = makeImpTV ();
    in
      val dbListCreateType : types = (Unit ->> a) ** (List a) ->> DbList a;
    end;
    
    local (* a normal type variable *)
      val a : types = makeTV ();
    in
      val dbListToListType : types = DbList a ->> List a;
    end;
  in
    val U : unit = #enterType PolyMLEnv ("dbList", dbListType);
    val U : unit = enterPolyML ("dbListCreate", mkConst (toWord dbListCreate), dbListCreateType);
    val U : unit = enterPolyML ("dbListToList", mkConst (toWord dbListToList), dbListToListType);
   end;

(*****************************************************************************)
(*                  PolyML.unsafeCast                                        *)
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
    val U : unit =
      enterPolyML ("unsafeCast", unsafeCastEntry, unsafeCastType);
  end;
  

(*****************************************************************************)
(*                  PolyML.run_call*                                         *)
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
   
     PolyML.Inner.run_call1 POLY_SYS_io_operation
     
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
    val U : unit = enterPolyML ("run_call0", runCall0Entry, runCall0Type);
    val U : unit = enterPolyML ("run_call1", runCall1Entry, runCall1Type);
    val U : unit = enterPolyML ("run_call2", runCall2Entry, runCall2Type);
    val U : unit = enterPolyML ("run_call3", runCall3Entry, runCall3Type);
    val U : unit = enterPolyML ("run_call4", runCall4Entry, runCall4Type);
    val U : unit = enterPolyML ("run_call5", runCall5Entry, runCall5Type);
  end;

(*****************************************************************************)
(*                  Initialisation and bootstrapping functions                                 *)
(*****************************************************************************)
  local
    fun addVal (name : string, value : 'a, typ : types) : unit =
      enterPolyML (name, mkConst (toWord value), typ)
  in
    val U : unit = addVal ("initialFn", initialFn, Ref (Bool ->> Unit));
    val U : unit = addVal ("startupFn", startupFn, Ref (Unit ->> Unit));
	(* When we start the compiler we don't have any conversion functions.
	   We can't even use a literal string until we have installed a
	   basic converter. *)
    val U : unit = addVal ("convStringName", "convString", String);
    val U : unit = addVal ("convInt", intOfString, String ->> Int);
	(* Convert a string, recognising and converting the escape codes. *)
    val U : unit = addVal ("convString", unescapeString, String ->> String);
	(* References to functions for interactive input and output.  We need to
	   change these to use TextIO.stdIn/stdOut once we have compiled the
	   basis library.  That allows the redirection to work. *)
	val U : unit = addVal ("printString", DEBUG.printString, Ref(String->>Unit));
	val U : unit = addVal ("inputChar", DEBUG.inputChar, Ref(Unit->>Option Char));
	val U : unit = addVal ("nextIsNewline", DEBUG.nextIsNewline, Ref(Unit->>Bool));
 	val U : unit = addVal ("flushInput", DEBUG.flushInput, Ref(Unit->>Unit));
end;

(*****************************************************************************)
(*                  Polymorphic functions                                    *)
(*****************************************************************************)
   local
     val eqType = let val a = makeEqTV () in a ** a ->> Bool end;
     val eqVal  = mkSpecialFun "=" eqType Equal;
   in
     val U : unit = enterGlobalValue ("=", eqVal);
   end;        
    

   local
     val neqType = let val a = makeEqTV () in a ** a ->> Bool end;
     val neqVal  = mkSpecialFun "<>" neqType NotEqual;
   in
     val U : unit = enterGlobalValue ("<>", neqVal);
   end;        
    
(*****************************************************************************)
(*              Funny polymorphic functions (in structure PolyML.Inner)      *)
(*****************************************************************************)
   (* Inner contains operations that are passed through to the top level
      unchanged, others are system functions that are filtered out by prelude2. *)
    local
      val Inner = makeEmptyGlobal "Inner";
    in
      val U : unit       = #enterStruct PolyMLEnv ("Inner", Inner);
      val (Env InnerEnv) = makeEnv (structSignat Inner);
      val enterInnerVal  = #enterVal InnerEnv;
    end;

    fun enterInner (name : string, entry : codetree, typ : types) : unit =
    let
      val value = mkGvar (name, typ, entry);
    in
      enterInnerVal (name, value)
    end;

   local
     val printType = let val a = makeTV () in a ->> a end;
     val printVal  = mkSpecialFun "print" printType Print;
   in
     val U : unit = enterInnerVal ("print", printVal);
   end;
   
   local
     val makeStringType = let val a = makeTV () in a ->> String end;
     val makeStringVal  = mkSpecialFun "makestring" makeStringType MakeString;
   in
     val U : unit = enterInnerVal ("makestring", makeStringVal);
   end;
   
   local
     val addOverloadType =
	 	let val a = makeTV () and b = makeTV () in (a ->> b) ->> String ->> Unit end;
     val addOverloadVal  = mkSpecialFun "addOverload" addOverloadType AddOverload;
   in
     val U : unit = #enterVal PolyMLEnv ("addOverload", addOverloadVal);
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
     val U : unit = enterInnerVal ("install_pp", installPPVal);
   end;

(*****************************************************************************)
(*                  PolyML.profiling etc.                                    *)
(*****************************************************************************)
   local
     fun setVbl (vbl: 'a dbRef) (n : 'a) : unit = dbRefPut (vbl, n);

     fun putSetFunction (name : string) (typ : types) (vbl: 'a dbRef) : unit = 
     let
       val setEntry = mkConst (toWord (setVbl vbl : 'a -> unit));
       val setType  = typ ->> Unit;
     in
       enterInner (name, setEntry, setType)
     end;
   in
     val U : unit = putSetFunction "profiling"   Int  DEBUG.profiling;
     val U : unit = putSetFunction "timing"      Bool DEBUG.timing;
     val U : unit = putSetFunction "print_depth" Int  DEBUG.printDepth;
     val U : unit = putSetFunction "error_depth" Int  DEBUG.errorDepth;
   end;

   local
     fun getVbl (vbl: 'a dbRef) : unit -> 'a = 
       fn () => dbRefGet vbl;

     fun putGetFunction (name : string) (typ : types) (vbl : 'a dbRef) : unit = 
     let
       val getEntry = mkConst (toWord (getVbl vbl : unit -> 'a));
       val getType  = Unit ->> typ;
       val getVal   = mkGvar (name, getType, getEntry)
     in
       enterInner (name, getEntry, getType)
     end;
   in
     val U : unit = putGetFunction "get_profiling"   Int  DEBUG.profiling;
     val U : unit = putGetFunction "get_timing"      Bool DEBUG.timing;
     val U : unit = putGetFunction "get_print_depth" Int  DEBUG.printDepth;
     val U : unit = putGetFunction "get_error_depth" Int  DEBUG.errorDepth;
   end;

(*****************************************************************************)
(*        PolyML.{depends, make, use, compiler}                              *)
(*****************************************************************************)

(******************************************************************************
The following functions all have globalTable bound in. This means that calling
"use" in the secondary shell will side-effect the primary shell's environment,
which is pretty sad. Unfortunately, I can't think of a way to get around
this. Perhaps we should just make the shells explicitly share the
same environment?

SPF 1/6/95
******************************************************************************)
  local
    fun addEnvFn (typ : types) (name : string, f : dbEnv -> 'a -> 'b) : unit =
    let
      val value : 'a -> 'b = f globalTable;
    in
      enterInner (name, mkConst (toWord value), typ)
    end;
  
    val stringToUnit : types = String ->> Unit;
    val unitToString : types = Unit ->> String;
    
    val compilerType : types = 
      unitToString ** stringToUnit ->> Unit ->> Unit;
  in
    val U : unit = applyList (addEnvFn stringToUnit)
      [
        ("depends", dependsInEnv),
        ("make",    makeIntoEnv),
        ("use",     useIntoEnv)
      ];
      
    val U : unit = 
      addEnvFn compilerType ("compiler", runCompiler);
   end;


(*****************************************************************************)
(*                  PolyML.logging etc.                                      *)
(*****************************************************************************)
   local
     val
       {
		 shell:      unit -> unit,    (* The command processor *)
		 debugger:	 (string -> values)*(unit->bool) -> unit,
		 startLog:   string -> unit,  (* Starts logging commands to a file *)
		 restartLog: string -> unit,  (* Appends logging commands to a file *)
		 stopLog:    unit -> unit,    (* Stops logging *)
		 writeLog:   string -> unit,  (* Writes a string to the log file *)
		 logName:    unit -> string,  (* Get the log name *)
		 isLogging:  unit -> bool     (* true if logging, otherwise false *)
	}
	  = shellProc (globalTable, prompt1, prompt2);
     
     val startLogEntry   = mkConst (toWord startLog);
     val startLogType    = String ->> Unit;
     
     val restartLogEntry = mkConst (toWord restartLog);
     val restartLogType  = String ->> Unit;
     
     val stopLogEntry    = mkConst (toWord stopLog);
     val stopLogType     = Unit ->> Unit;
     
     val writeLogEntry   = mkConst (toWord writeLog);
     val writeLogType    = String ->> Unit;
     
     val logNameEntry    = mkConst (toWord logName);
     val logNameType     = Unit ->> String;
     
     val isLoggingEntry  = mkConst (toWord isLogging);
     val isLoggingType   = Unit ->> Bool;
   in
     val U : unit = enterInner ("startLog",   startLogEntry,   startLogType);
     val U : unit = enterInner ("restartLog", restartLogEntry, restartLogType);
     val U : unit = enterInner ("stopLog",    stopLogEntry,    stopLogType);
     val U : unit = enterInner ("writeLog",   writeLogEntry,   writeLogType);
     val U : unit = enterInner ("logName",    logNameEntry,    logNameType);
     val U : unit = enterInner ("isLogging",  isLoggingEntry,  isLoggingType);
     val sessionShell = shell; (* save for later! *)
   end;

(*****************************************************************************)
(*                  PolyML.make_database                                     *)
(*****************************************************************************)
   local
     fun makeDb (name: string) : unit = 
       createDatabase (name, rootShell, systemCommit);
     
     val makeDbEntry = mkConst (toWord makeDb);
     val makeDbType  = String ->> Unit;
   in
     val U : unit = enterInner ("make_database", makeDbEntry, makeDbType);
   end;

(*****************************************************************************)
(*                  PolyML.commit                                            *)
(*****************************************************************************)
   local
     fun userCommit () : bool =
     let
       val b : bool = systemCommit ();
       val U : unit = if b then () else (!startupFn) ();
     in
       b
     end;
     
     val commitEntry = mkConst (toWord userCommit);
     val commitType  = Unit ->> Bool;
   in
     val U : unit = enterInner ("commit", commitEntry, commitType);
   end;
   
(*****************************************************************************)
(*                  Exceptions                                               *)
(*****************************************************************************)
   local
     fun addInnerExn (argtyp : types) (name : string, id : int) : unit =
     let
       val exConst = mkConst (toWord id);
       val exVal   = mkGex (name, argtyp, exConst);
     in
       enterInnerVal (name, exVal)
     end 
   in
     val U : unit = applyList (addInnerExn String)
       [
        ("Commit",     EXC_commit),      (*   3 *)
		("XWindows",   EXC_XWindows),    (*  10 *)
        ("Foreign",    EXC_foreign)      (*  23 *)
       ];
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
    val U : unit = enterGlobalValue ("+", mkOverloaded "+"   addType);
    val U : unit = enterGlobalValue ("-", mkOverloaded "-"   addType);
    val U : unit = enterGlobalValue ("*", mkOverloaded "*"   addType);
    val U : unit = enterGlobalValue ("~", mkOverloaded "~"   negType);
    val U : unit = enterGlobalValue ("abs", mkOverloaded "abs" negType);
    val U : unit = enterGlobalValue (">=", mkOverloaded ">="  cmpType);
    val U : unit = enterGlobalValue ("<=", mkOverloaded "<="  cmpType);
    val U : unit = enterGlobalValue (">", mkOverloaded ">"   cmpType);
    val U : unit = enterGlobalValue ("<", mkOverloaded "<"   cmpType);
	(* The following overloads are added in ML97 *)
    val U : unit = enterGlobalValue ("div", mkOverloaded "div"   addType);
    val U : unit = enterGlobalValue ("mod", mkOverloaded "mod"   addType);
    val U : unit = enterGlobalValue ("/", mkOverloaded "/"   addType);
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
         
       val argSig0 : signatures = makeCopy ("", sig0, 0, 0);

       val (Env argEnv) = makeEnv argSig0;
       
       val exIdenVal : values = makeFormalV ("ex_iden", Int, 0);

       val U : unit = #enterVal argEnv ("ex_iden", exIdenVal);
     in
       val argSig0 = argSig0;
     end
       
           
     (* Result signature. *)
     local
       (* make an anonymous, empty signature *)
       val sig0    = makeSignatures ""
       val resSig0 = makeCopy ("", sig0, 0, 0);
       
       val (Env resEnv) = makeEnv resSig0;
 
       val exType = emptyType;
       val exVal = makeFormalEx ("ex", exType, 0);
          
       val U : unit = #enterVal resEnv ("ex", exVal);
     in
        val resSig0 = resSig0;
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
     val U : unit = #enterFunct PolyMLEnv ("Run_exception0", Run_exception0);
   end;
   

(*****************************************************************************)
(*                  Funny functor PolyML.Run_exception1                      *)
(*****************************************************************************)
   (* functor Run_exception1(sig type exType; val ex_iden: int end) :
                             sig exception ex of exType end *) 
   local
     (* Make a nullary type constructor (the type of the exception) *)
     val exTypeConstr = 
       makeTypeConstrs ("ex_type", [], emptyType, makeBoundId 0, false, 0);

     (* Argument signature. *)
     local
       (* make an anonymous, empty signature *)
       val sig1 = makeSignatures "";
           
       (* Contains 1 bound type *)
       val argSig1 = makeCopy ("", sig1, 0, 1);

       val (Env argEnv) = makeEnv argSig1;
       
       val exIdenVal = makeFormalV ("ex_iden", Int, 0);

       val U : unit = #enterType argEnv ("ex_type", exTypeConstr);
       val U : unit = #enterVal  argEnv ("ex_iden", exIdenVal);
     in
       val argSig1 = argSig1;
     end

     (* Result signature. *)
     local
       (* make an anonymous, empty signature *)
       val sig1 = makeSignatures "";
           
       (* 1 bound type inherited from argument sig *)
       val resSig1 = makeCopy ("", sig1, 0, 1);

       val (Env resEnv) = makeEnv resSig1;
       
     (* get the actual type from the nullary type constructor *)
     val exType = mkTypeConstruction ("ex_type", exTypeConstr, []);
     val exVal  = makeFormalEx ("ex", exType, 0);
          
       val U : unit = #enterVal resEnv ("ex", exVal);
     in
        val resSig1 = resSig1;
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
     val U : unit = #enterFunct PolyMLEnv ("Run_exception1", Run_exception1);
   end;
   
(*****************************************************************************)
(*                  PolyML.Compiler structure                                *)
(*****************************************************************************)
    local
      val Compiler = makeEmptyGlobal "Compiler";
    in
      val U : unit          = #enterStruct InnerEnv ("Compiler", Compiler);
      val (Env CompilerEnv) = makeEnv (structSignat Compiler);
      val enterCompilerVal  = #enterVal CompilerEnv;
    end;

    fun enterCompilerConst (name : string, value : word, typ : types) : unit =
    let
      val value = mkGvar (name, typ, mkConst value);
    in
      enterCompilerVal (name, value)
    end;

(*****************************************************************************)
(*                  PolyML.Compiler entries copied from DEBUG                *)
(*****************************************************************************)
   local
     open DEBUG;
   in
     val U : unit = applyList enterCompilerConst
       [
         ("prompt1",             toWord prompt1,             Ref String),
         ("prompt2",             toWord prompt2,             Ref String),
         ("useRCS",              toWord useRCS,              Ref Bool),
         ("suffixes",            toWord suffixes,            Ref (List String)),
         ("maxInlineSize",       toWord maxInlineSize,       Ref Int),
         ("assemblyCode",        toWord assemblyCode,        Ref Bool),
         ("parsetree",           toWord parsetree,           Ref Bool),
         ("codetree",            toWord codetree,            Ref Bool),
         ("pstackTrace",         toWord pstackTrace,         Ref Bool),
         ("codetreeAfterOpt",    toWord codetreeAfterOpt,    Ref Bool),
         ("traceCompiler",       toWord traceCompiler,       Ref Bool),
         ("inlineFunctors",      toWord inlineFunctors,      Ref Bool),
         ("ml90",        		 toWord ml90,        		 Ref Bool),
         ("debug",        		 toWord debug,        		 Ref Bool),
         ("printInAlphabeticalOrder",    toWord printInAlphabeticalOrder,     Ref Bool),
         ("printTypesWithStructureName", toWord printTypesWithStructureName,  Ref Bool)
      ]
   end;

(*****************************************************************************)
(*                  Debugger functions                                       *)
(*****************************************************************************)
   local
   	  open DEBUGGER
	  (* Create a new structure for them. *)
      val Debug = makeEmptyGlobal "Debug";
      val _ = #enterStruct InnerEnv ("Debug", Debug);
      val (Env DebugEnv) = makeEnv (structSignat Debug);
      val enterDebugVal  = #enterVal DebugEnv;

      fun enterDebugConst (name : string, value : word, typ : types) : unit =
      let
        val value = mkGvar (name, typ, mkConst value);
      in
        enterDebugVal (name, value)
      end;

	  val unitToUnit = Unit ->> Unit
	  val stringIntToUnit = (String ** Int) ->> Unit
	  val stringToUnit = String ->> Unit
   in
   	  val _ = enterDebugConst ("step", toWord singleStep, unitToUnit);
   	  val _ = enterDebugConst ("stepOver", toWord stepOver, unitToUnit);
   	  val _ = enterDebugConst ("stepOut", toWord stepOut, unitToUnit);
   	  val _ = enterDebugConst ("continue", toWord continue, unitToUnit);
   	  val _ = enterDebugConst ("breakAt", toWord addLineBreak, stringIntToUnit);
   	  val _ = enterDebugConst ("clearAt", toWord clearLineBreak, stringIntToUnit)
   	  val _ = enterDebugConst ("breakIn", toWord addFnBreak, stringToUnit);
   	  val _ = enterDebugConst ("clearIn", toWord clearFnBreak, stringToUnit)
   	  val _ = enterDebugConst ("up", toWord upStack, unitToUnit)
   	  val _ = enterDebugConst ("down", toWord downStack, unitToUnit)
   	  val _ = enterDebugConst ("variables", toWord printVars, unitToUnit)
   	  val _ = enterDebugConst ("stack", toWord stackTrace, unitToUnit)
   	  val _ = enterDebugConst ("dump", toWord dumpStack, unitToUnit)
   	  val _ = enterDebugConst ("trace", toWord traceFunctions, Bool ->> Unit)
   end


(*****************************************************************************)
(*                  Forget functions etc.                                    *)
(*****************************************************************************)
(* N.B. these functions - like "use" and "make", operate on globalTable,
   even if they are executed from within a subshell. This can give
   *very* peculiar results. SPF 31/5/95. *)
  local
    fun addEnvFn (typ : types) (name : string, f : dbEnv -> 'a -> 'b) : unit =
    let
      val value : 'a -> 'b = f globalTable;
    in
      enterCompilerConst (name, toWord value, typ)
    end;
  in
    val U : unit = applyList (addEnvFn (String ->> Unit))
      [
        ("forgetMakeInfo",  forgetMakeInfo),
        ("forgetSignature", forgetSignature),
        ("forgetStructure", forgetStructure),
        ("forgetFunctor",   forgetFunctor),
        ("forgetValue",     forgetValue),
        ("forgetType",      forgetType),
        ("forgetFixity",    forgetFixity)
      ];
      
    val U : unit = applyList (addEnvFn (Unit->> List String))
      [
        ("signatureNames",  signatureNames),
        ("structureNames",  structureNames),
        ("functorNames",    functorNames),
        ("valueNames",      valueNames),
        ("typeNames",       typeNames),
        ("fixityNames",     fixityNames)
      ];

    val U : unit = addEnvFn (String ->> List String)
      ("dependencies", dependenciesInEnv);
  end;
in
  (globalTable, sessionShell)
end (* initGlobalEnv *);


end;
