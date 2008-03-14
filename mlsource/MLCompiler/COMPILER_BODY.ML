(*
	Copyright (c) 2000
		Cambridge University Technical Services Limited

    Modified David C.J. Matthews 2008.

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
    Title:      Body of ML Compiler.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)


functor COMPILER_BODY (

(*****************************************************************************)
(*                  SYMSET                                                   *)
(*****************************************************************************)
structure SYMSET :
sig
  type sys
  type symset
  
  val inside: sys * symset -> bool;
  val ++ :    symset * symset -> symset
  val abortParse:   symset
  val semicolon:    symset
end;

(*****************************************************************************)
(*                  LEX                                                      *)
(*****************************************************************************)
structure LEX :
sig
  type lexan
  type sys
  type prettyPrinter
     
  val lexPrint : lexan -> string -> unit;
     
  val insymbol: lexan -> unit
  val sy:       lexan -> sys
  val errorOccurred: lexan -> bool
  val resetLexan:    lexan -> unit
  val flushLexan:    lexan -> unit;

  val initial: (unit -> char option) * string * (unit -> int) * (string -> unit) -> lexan;
end;

(*****************************************************************************)
(*                  CODETREE                                                 *)
(*****************************************************************************)
structure CODETREE :
sig
  type codetree
  type prettyPrinter
  
  val genCode:   codetree -> unit -> codetree
  val pretty:    codetree * prettyPrinter -> unit;
end;

(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS :
sig
  type structVals
  type signatures
  type fixStatus
  type functors
  type typeConstrs;
  
  type types
  val isEmpty:          types -> bool

  type values
  val isValueConstructor:      values -> bool
  
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
end; 

(*****************************************************************************)
(*                  VALUEOPS                                                 *)
(*****************************************************************************)
structure VALUEOPS :
sig
  type prettyPrinter
  type values
  type structVals
  type functors
  type signatures
  type fixStatus
  type word
  
  val exnName : exn -> string
  
  val displayFixStatus:  fixStatus  * int * prettyPrinter -> unit
  val displaySignatures: signatures * int * prettyPrinter -> unit
  val displayStructures: structVals * int * prettyPrinter -> unit
  val displayFunctors:   functors   * int * prettyPrinter -> unit
  val displayValues:     values * int * prettyPrinter-> unit

  val findException: (values list * structVals list) -> word -> values option

  val printerEnvironment:
      { lookupFix: string -> fixStatus option, lookupExnById: word -> values option} ref

end;


(*****************************************************************************)
(*                  TYPETREE                                                 *)
(*****************************************************************************)
structure TYPETREE :
sig
  type types
  type typeConstrs
  type prettyPrinter
  val displayTypeConstrs: typeConstrs * int * prettyPrinter -> unit
  val exnType:    types
end;


(*****************************************************************************)
(*                  STRUCTURES                                               *)
(*****************************************************************************)
structure STRUCTURES :
sig
  type structs
  type lexan
  type prettyPrinter
  type codetree
  type env

  type values
  type structVals
  type functors
  type fixStatus
  type typeConstrs
  type signatures

  val pass2Structs: structs list * lexan * env -> unit
  val checkForFreeTypeVars:
  	((string*values->unit)->unit) * ((string*structVals->unit)->unit) *
		((string*functors->unit)->unit) * lexan -> unit
  val pass4Structs:  
    codetree * structs list *
     {
       enterVal:     string * values      -> unit,
       enterStruct:  string * structVals  -> unit,
       enterFunct:   string * functors    -> unit,
       enterFix:     string * fixStatus   -> unit,
       enterType:    string * typeConstrs -> unit,
       enterSig:     string * signatures  -> unit
     } -> unit;
  val gencodeStructs: structs list * lexan -> codetree;
  val displayStructs: structs list * int * prettyPrinter -> unit
end;

(*****************************************************************************)
(*                  PARSEDEC                                                 *)
(*****************************************************************************)
structure PARSEDEC :
sig
  type lexan
  type symset
  type fixStatus
  type structs
  
  val parseDec: symset * lexan *
   { enterFix:  string * fixStatus -> unit,
     lookupFix: string -> fixStatus option }
     -> structs list
end;

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
   val parsetree   : bool ref
   val codetree    : bool ref
   
   val timing      : bool ref
   val profiling   : int ref
   val errorDepth  : int ref
   val printDepth  : int ref
   val lineLength  : int ref
   
   val traceCompiler : bool ref
   val printString:   (string->unit) ref
   val printInAlphabeticalOrder : bool ref
end;

(*****************************************************************************)
(*                  UTILITIES                                                *)
(*****************************************************************************)
structure UTILITIES :
sig
  val searchList: unit ->
    { apply: (string * 'a -> unit) -> unit,
      enter:  string * 'a -> unit,
      lookup: string -> 'a  option}
end;

(*****************************************************************************)
(*                  PRETTYPRINTER                                            *)
(*****************************************************************************)
structure PRETTYPRINTER :
sig
 type prettyPrinter
 
  val ppAddString  : prettyPrinter -> string -> unit
  val ppBeginBlock : prettyPrinter -> int * bool -> unit
  val ppEndBlock   : prettyPrinter -> unit -> unit
  val ppBreak      : prettyPrinter -> int * int -> unit
  val ppLineBreak  : prettyPrinter -> unit -> unit
  val ppEndStream  : prettyPrinter -> unit -> unit
  
  val prettyPrint : int * (string -> unit) -> prettyPrinter; 
end;

(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
sig
  exception InternalError of string
  val lookupDefault : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option
  val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list
end;

(*****************************************************************************)
(*                  POLYML                                                   *)
(*****************************************************************************)
structure POLYML :
sig
  val exception_trace : (unit -> 'a) -> 'a;
end;

(*****************************************************************************)
(*                  RUNCALL                                                  *)
(*****************************************************************************)
structure RUNCALL :
sig
  val run_call1 : int -> 'a -> 'b;
end;

(*****************************************************************************)
(*                  COMPILERBODY sharing constraints                         *)
(*****************************************************************************)

sharing type
  PRETTYPRINTER.prettyPrinter
= LEX.prettyPrinter
= STRUCTURES.prettyPrinter
= VALUEOPS.prettyPrinter
= CODETREE.prettyPrinter
= TYPETREE.prettyPrinter;

sharing type
  SYMSET.sys
= LEX.sys

sharing type
  SYMSET.symset
= PARSEDEC.symset

sharing type
  LEX.lexan
= PARSEDEC.lexan
= STRUCTURES.lexan

sharing type
  STRUCTVALS.fixStatus
= PARSEDEC.fixStatus
= VALUEOPS.fixStatus
= STRUCTURES.fixStatus

sharing type
  PARSEDEC.structs
= STRUCTURES.structs

sharing type
  STRUCTURES.codetree
= CODETREE.codetree

sharing type
  STRUCTVALS.env
= STRUCTURES.env

sharing type
  TYPETREE.typeConstrs
= STRUCTVALS.typeConstrs
= STRUCTURES.typeConstrs

sharing type
  VALUEOPS.values
= STRUCTVALS.values
= STRUCTURES.values

sharing type
  VALUEOPS.structVals
= STRUCTVALS.structVals
= STRUCTURES.structVals

sharing type
  VALUEOPS.functors
= STRUCTVALS.functors
= STRUCTURES.functors

sharing type
  VALUEOPS.signatures
= STRUCTVALS.signatures
= STRUCTURES.signatures

(*****************************************************************************)
(*                  COMPILERBODY export signature                            *)
(*****************************************************************************)

 ) : 
 
sig
    type values;
    type typeConstrs;
    type fixStatus;
    type structVals;
    type signatures;
    type functors;

    type nameSpace =
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
        enterFunct:   string * functors    -> unit,

        allVal:       unit -> (string*values) list,
        allType:      unit -> (string*typeConstrs) list,
        allFix:       unit -> (string*fixStatus) list,
        allStruct:    unit -> (string*structVals) list,
        allSig:       unit -> (string*signatures) list,
        allFunct:     unit -> (string*functors) list
      };

    (* The completed compiler. *)
    val compiler :
        { nameSpace: nameSpace, getChar: unit->char option, fileName: string,
          lineNumber: unit->int, putString: string->unit }
       -> unit -> unit;         (* the function to actually execute *)

    (* Function to set the environment used by PolyML.print etc to look
       up infix status and exceptions.  *)
    val setPrintEnv : nameSpace -> unit
      
end =

(*****************************************************************************)
(*                  COMPILERBODY functor body                                *)
(*****************************************************************************)
struct
  open MISC;
  open STRUCTVALS;
  open PRETTYPRINTER;
  open LEX;
  open DEBUG;

  fun printTimes (parseTime,pass2Time,treeTime,codeTime,endRunTime) : unit =
    if not (! timing) then ()
    else let
      val print = (!printString)
      fun printTime t = print(Time.fmt 1 t)
    in
      print "Timing - parse:";
      printTime(parseTime);
      print ", semantics:";
      printTime(pass2Time-parseTime);
      print ", translate:";
      printTime(treeTime-pass2Time);
      print ", generate:";
      printTime(codeTime-treeTime);
      print ", run:";
      printTime(endRunTime);
      print "\n"
   end

  
  (* switch profiling on/off *)
  val systemProfile : int -> unit =
    RUNCALL.run_call1 RuntimeCalls.POLY_SYS_profiler;
  
  fun startProfile (level : int) : unit = 
    if level <> 0 then systemProfile level else ();
      
  fun stopProfile (level : int) : unit = 
    if level <> 0 then systemProfile 0 else ();

  val stopSyms =
    let
      open SYMSET
    in
      op ++ (abortParse, semicolon)
    end;

  fun baseCompiler (lex : lexan, STRUCTVALS.Env globals) : unit -> unit =
  let (* let1 *)

	(* Return the total user+system time since the start. *)
	fun cpuTime start =
	let
		val {usr, sys} = Timer.checkCPUTimer start
	in
		usr+sys
	end

    val startTime = Timer.startCPUTimer();
    
    (* Clear lexer error state (but NOT look-ahead state) *)
    val U : unit = resetLexan lex;
    
    (* 
       Ignore semicolon (if any) from the previous declaration. The
       very first token produced by the lexer is a semicolon, which
       makes life easier. DON'T make this a loop because the prompt
       only gets reset on a new call to "compiler". This means that
       if we try to process more than one (empty) statement at a time,
       we'll get the wrong (secondary) prompt!
       SPF 19/4/96
    *)
    val U : unit =
      if SYMSET.inside (sy lex, SYMSET.semicolon)
      then insymbol lex
      else ();
  in (* let1 *)
    (* An empty declaration (or end of file!) *)
    if SYMSET.inside (sy lex, stopSyms)
    then if errorOccurred lex (* We could have, for example, an unterminated comment. *)
       then (resetLexan lex; raise Fail "Static errors (pass 1)")
       else fn () => () (* Do nothing *)
    else let (* let2 *)
      (* create a "throw away" compiling environment for this topdec *)
      val newFixEnv = UTILITIES.searchList ();
      val enterFix  = #enter newFixEnv;
      val lookupFix = lookupDefault (#lookup newFixEnv) (#lookupFix globals);
   
      (* parse a topdec *)
      val parseTree : STRUCTURES.structs list = 
        PARSEDEC.parseDec (stopSyms, lex,
          {enterFix  = enterFix,
           lookupFix = lookupFix});

      val parseTime = cpuTime startTime;
      val UUU:unit = 
        if !parsetree
        then let
          val pstream = prettyPrint (77,lexPrint lex);
        in
          ppBeginBlock pstream (0, false);
          STRUCTURES.displayStructs (parseTree, ! printDepth, pstream);
          ppEndBlock pstream ()
        end
        else ();
 
      val U : unit =
       if errorOccurred lex
       then (resetLexan lex; raise Fail "Static errors (pass 1)")
       else ()
 
      (* extend throw-away compiling environment *)
      val newValEnv   = UTILITIES.searchList();
      val newTypeEnv  = UTILITIES.searchList();
      val newStrEnv   = UTILITIES.searchList();
      val newSigEnv   = UTILITIES.searchList();
      val newFuncEnv  = UTILITIES.searchList();
   
      (* create an "env" from the throw-away environment *)
      val lookupVal =
        lookupDefault (#lookup newValEnv)  (#lookupVal globals);
         
      val lookupType =
        lookupDefault (#lookup newTypeEnv) (#lookupType globals);
         
      val lookupStruct =
        lookupDefault (#lookup newStrEnv)  (#lookupStruct globals);
        
      val lookupSig =
        lookupDefault (#lookup newSigEnv)  (#lookupSig globals);
 
      val lookupFunct =
        lookupDefault (#lookup newFuncEnv) (#lookupFunct globals);
 
     (* For each declaration we enter a reference to the identifier in this
        table. We can then construct code which will load each value into 
        a vector which can be returned. Used for variables and exceptions. *)
      val locals  = 
          {
           lookupVal     = lookupVal,
           lookupType    = lookupType,
           lookupFix     = lookupFix,
           lookupStruct  = lookupStruct,
           lookupSig     = lookupSig,
           lookupFunct   = lookupFunct,
           enterVal      = #enter newValEnv,
           enterType     = #enter newTypeEnv,
           enterFix      = enterFix,
           enterStruct   = #enter newStrEnv,
           enterSig      = #enter newSigEnv,
           enterFunct    = #enter newFuncEnv
          };
 
      val localEnv : env = STRUCTVALS.Env locals;
 
      (* If no errors then do second pass to match identifiers
         and declarations and return type of expression. *)
      val UUU:unit = STRUCTURES.pass2Structs (parseTree, lex, localEnv);

	  (* We need to check that no top-level value has a free type
	     variable in its type.  We can only do that once unification
		 is complete because one declaration may "freeze" the type of
		 another within a single "topdec".
		 Don't do this check if we've already reported an error.
		 Mistyped top-level functions are given type ('a) which then
		 would be reported as an error here. *)
	  val U: unit =
	  	if errorOccurred lex then ()
	  	else STRUCTURES.checkForFreeTypeVars(#apply newValEnv, #apply newStrEnv,
							 #apply newFuncEnv, lex);
 
      val pass2Time = cpuTime startTime;
 
      val U : unit =
        if errorOccurred lex
        then (resetLexan lex; raise Fail "Static errors (pass2)")
        else ();
      
      (* Only code-generate if there were no errors and
         it's not a directive. *)
      val optimisedCode : CODETREE.codetree =
        STRUCTURES.gencodeStructs (parseTree, lex);
      val treeTime = cpuTime startTime;
      
      val U : unit =
        if errorOccurred lex
        then (* Errors can be produced during the code-generation phase. *)
           (resetLexan lex; raise Fail "Static errors (pass3)")
        else ();
 
       val UUU:unit =
         if !Debug.codetree
         then let
           val pstream = prettyPrint (77,lexPrint lex);
         in
           ppBeginBlock pstream (0, false);
           CODETREE.pretty (optimisedCode, pstream);
           ppEndBlock pstream ()
         end
         else ();
      
      val resultCode : unit -> CODETREE.codetree =
         CODETREE.genCode optimisedCode;
      val codeTime = cpuTime startTime;
     
    in (* let2 *)
     (* This is the procedure which is returned as the result of
         the compilation. *)
     fn () => 
       let (* let3 *)
         (* Save the value of the profiling switch,
             and switch profiling on if required. *)
         val wasProfiling = ! profiling;

         val startRunTime = Timer.startCPUTimer(); (* Save the run-time *)
         val U : unit = startProfile wasProfiling;

         val resultVec = (* Now run it *)
             resultCode ()
                 handle exn => (* Exceptions in run-time code. *)
                 let
                     val () = stopProfile wasProfiling;
                     val endRunTime = cpuTime startRunTime;
                     (* if we are profiling, stop and force out the profile counts *)
                     val () = stopProfile wasProfiling;
                 in
                     (* print out timing info *)
                     printTimes (parseTime,pass2Time,treeTime,codeTime,endRunTime);
                     raise exn
                 end
 
         val () = stopProfile wasProfiling;
         val endRunTime = cpuTime startRunTime;

       in (* let3 *)
         if ! printDepth <= 0
         then let (* Don't print anything. *)
             (* Just add the entries to the global declarations. *)
            val pass4Env = 
              { 
                enterVal    = #enterVal    globals,
                enterStruct = #enterStruct globals,
                enterFunct  = #enterFunct  globals,
                enterFix    = #enterFix    globals,
                enterType   = #enterType   globals,
                enterSig    = #enterSig    globals
              };
         in
           (* install new types *)
           #apply newTypeEnv (#enterType globals);

           (* install new fixities *)
           #apply newFixEnv (#enterFix globals);
                  
           (* install new values, structures and functors *)
           STRUCTURES.pass4Structs (resultVec, parseTree, pass4Env);

           (* install new signatures *)
           #apply newSigEnv (#enterSig globals)
         end
         
         else let  (* Now print out the results. *)
		 (* It's much easier to read if we can see the declarations in some sort
		    of order so we accumulate them into a list, sort it and print it out. *)
			datatype printKind =
				TypeConstrKind of typeConstrs
			|	FixStatusKind of fixStatus
			|	SignatureKind of signatures
			|	StructureKind of structVals
			|	FunctorKind of functors
			|	ValueKind of values

			val declist = ref(nil: (string*printKind)list);
        in

           (* Values, structures and functors. *)
           let
             fun enterVal (name: string, dec: values) =
             (* Constructors are printed as part of the type. 
                Actually they aren't if we have redefined the type
                e.g. datatype X=a type X=X.  Should something be done
                about this?  The only "overloaded" values we can get in
                here are if we open PolyML when "print" and "makestring"
                are added to the name space. *)
               (
                 if not (isValueConstructor dec)
                 then declist := (name, ValueKind dec) :: !declist else ();
                 #enterVal globals (name, dec)
               );

			 fun enterToList constr globalEnter (pair as (name, dec)) =
               (
			     declist := (name, constr dec) :: !declist;
                 globalEnter globals pair
               );

             fun enterStruct (name: string, dec: structVals) =
               (
			     declist := (name, StructureKind dec) :: !declist;
                 #enterStruct globals (name, dec)
               );

             fun enterFunct (name: string, dec: functors) =
               (
			     declist := (name, FunctorKind dec) :: !declist;
                 #enterFunct globals (name, dec)
               );

            val pass4Env  = 
              { 
                enterVal    = enterVal,
                enterStruct = enterToList StructureKind #enterStruct,
                enterFunct  = enterToList FunctorKind #enterFunct,
                enterFix    = enterToList FixStatusKind #enterFix,
                enterType   = enterToList TypeConstrKind #enterType,
                enterSig    = enterToList SignatureKind #enterSig
               };
           in
             STRUCTURES.pass4Structs (resultVec, parseTree, pass4Env)
           end;
                

		   let
	        val ll = ! lineLength;
            val pstream = prettyPrint (if ll < 10 then 10 else ll, lexPrint lex);

		    fun printEntry (_, TypeConstrKind t) =
				TYPETREE.displayTypeConstrs (t, ! printDepth, pstream)
			|	printEntry (name, FixStatusKind f) =
				 (
				 ppBeginBlock pstream (0, false);
                 VALUEOPS.displayFixStatus (f, ! printDepth, pstream);
                 ppBreak pstream (1, 0);
                 ppAddString pstream name;
				 ppEndBlock pstream ()
				 )
			|   printEntry (_, SignatureKind s) =
                 VALUEOPS.displaySignatures (s, ! printDepth, pstream)
			|   printEntry (_, StructureKind s) =
                 VALUEOPS.displayStructures (s, ! printDepth, pstream)
			|   printEntry (_, FunctorKind f) =
                 VALUEOPS.displayFunctors (f, ! printDepth, pstream)
			|   printEntry (_, ValueKind v) =
                   VALUEOPS.displayValues (v, ! printDepth, pstream);

			(* Primarily sort by name, secondarily by type of entry. *)
			fun kindToInt(FixStatusKind _) = 0
			|   kindToInt(TypeConstrKind _) = 1
			|   kindToInt(SignatureKind _) = 2
			|   kindToInt(StructureKind _) = 3
			|   kindToInt(FunctorKind _) = 4
			|   kindToInt(ValueKind _) = 5
			fun order (s1: string, k1) (s2, k2) =
					if s1 = s2 then kindToInt k1 <= kindToInt k2
					else s1 <= s2
			(* Don't sort the declarations if we want them in declaration order. *)
			(* There's still a problem here.  We don't remove duplicates. *)
			val sortedDecs =
				if !printInAlphabeticalOrder then quickSort order (! declist) else List.rev (! declist)
		   in
 		    List.app printEntry sortedDecs
		   end
        end;

       (* print out timing info *)
       printTimes (parseTime,pass2Time,treeTime,codeTime,endRunTime)

      end (* let3 *)
    end (* let2 *)
  end; (* let1 *)

    type nameSpace =
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
        enterFunct:   string * functors    -> unit,

        allVal:       unit -> (string*values) list,
        allType:      unit -> (string*typeConstrs) list,
        allFix:       unit -> (string*fixStatus) list,
        allStruct:    unit -> (string*structVals) list,
        allSig:       unit -> (string*signatures) list,
        allFunct:     unit -> (string*functors) list
      };

    fun compiler { nameSpace: nameSpace, getChar: unit->char option, fileName: string,
          lineNumber: unit->int, putString: string->unit } : unit -> unit =
    let
        val lex = LEX.initial(getChar, fileName, lineNumber, putString);

        (* Convert the environment into the form used by the compiler. *)
        val useEnv : STRUCTVALS.env =
          STRUCTVALS.Env
            { 
              lookupFix    = #lookupFix nameSpace,
              lookupVal    = #lookupVal nameSpace,
              lookupType   = #lookupType nameSpace,
              lookupSig    = #lookupSig nameSpace,
              lookupStruct = #lookupStruct nameSpace,
              lookupFunct  = #lookupFunct nameSpace,
              enterFix     = #enterFix nameSpace,
              enterVal     = #enterVal nameSpace,
              enterType    = #enterType nameSpace,
              enterStruct  = #enterStruct nameSpace,
              enterSig     = #enterSig nameSpace,
              enterFunct   = #enterFunct nameSpace
             };
    in
    (
        if !traceCompiler
        then POLYML.exception_trace (fn () => baseCompiler(lex,useEnv))
        else baseCompiler(lex,useEnv)
    )
      handle
        SML90.Interrupt =>
        (
          LEX.flushLexan lex; (* No longer needed? *)
		  putString ("Compilation interrupted\n");
          raise SML90.Interrupt
        )
        
      | Fail s => 
		  raise Fail s (* static errors - caught by next level up *)
		 
      | InternalError s =>
		let
		  val s' =
		  "Exception- InternalError: " ^ String.toString s ^ " raised while compiling"
		in
		  putString (s' ^ "\n");
		  raise Fail s'
		end
       
      | exn =>
     	let
		  val s' =
		      "Exception- " ^ VALUEOPS.exnName exn ^ " unexpectedly raised while compiling"
		in
		  putString (s' ^ "\n");
		  raise Fail s'
		end
    end 

    fun setPrintEnv nameSpace: unit =
    let
        (* This function is bound into PolyML.print, PolyML.makeString and General.exnMessage
           to lookup exceptions.  This enables the contents of exception packets to be printed. *)
        fun exceptionSearch (w: VALUEOPS.word) : values option =
        let
            val values = #allVal nameSpace ()
            and strs   = #allStruct nameSpace ()
            (* Filter out the strings.  Order doesn't matter. *)
            fun getVal []            l = l
            |   getVal ((_, v) :: r) l = getVal r (v::l)
        in
            VALUEOPS.findException(getVal values [], getVal strs []) w
        end

    in
        VALUEOPS.printerEnvironment := { lookupFix = #lookupFix nameSpace, lookupExnById = exceptionSearch }
    end
end; (* struct *)
