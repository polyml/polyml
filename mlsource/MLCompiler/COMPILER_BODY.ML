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

structure SYMSET :
sig
  type sys
  type symset
  
  val inside: sys * symset -> bool;
  val ++ :    symset * symset -> symset
  val abortParse:   symset
  val semicolon:    symset
end;

structure LEX : LEXSIG

structure CODETREE :
sig
  type codetree
  type pretty
  
  val genCode:   codetree * Universal.universal list -> unit -> codetree
  val pretty:    codetree -> pretty;
end;

structure STRUCTVALS : STRUCTVALSIG; 

structure VALUEOPS : VALUEOPSSIG;

structure EXPORTTREE: EXPORTTREESIG

structure STRUCTURES : STRUCTURESSIG

structure PARSEDEC :
sig
  type lexan
  type symset
  type fixStatus
  type program
  
  val parseDec: symset * lexan *
    { enterFix:  string * fixStatus -> unit, lookupFix: string -> fixStatus option } -> program
end;

structure DEBUG :
sig
    val parsetreeTag   : bool Universal.tag
    val codetreeTag    : bool Universal.tag
    val timingTag      : bool Universal.tag
    val profilingTag   : int Universal.tag
    val traceCompilerTag : bool Universal.tag

    val getParameter :
       'a Universal.tag -> Universal.universal list -> 'a
end;

structure UTILITIES :
sig
  val searchList: unit ->
    { apply: (string * 'a -> unit) -> unit,
      enter:  string * 'a -> unit,
      lookup: string -> 'a  option}
end;

structure PRETTY : PRETTYSIG

structure MISC :
sig
  exception InternalError of string
  val lookupDefault : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option
  val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list
end;

sharing STRUCTVALS.Sharing = VALUEOPS.Sharing = PRETTY.Sharing = STRUCTURES.Sharing
      = LEX.Sharing = EXPORTTREE.Sharing = SYMSET = PARSEDEC = CODETREE

(*****************************************************************************)
(*                  COMPILERBODY export signature                            *)
(*****************************************************************************)

 ) : 
 
sig
    type values;
    type typeConstrSet;
    type fixStatus;
    type structVals;
    type signatures;
    type functors;
    type exportTree

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
      };

    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }

    (* The completed compiler.
       Returns the parse tree and, if successful, a function that executes the
       compiled code.  *)
    val compiler :
        nameSpace * (unit->char option) * Universal.universal list ->
        exportTree option * (unit ->
           { fixes: (string * fixStatus) list, values: (string * values) list,
             structures: (string * structVals) list, signatures: (string * signatures) list,
             functors: (string * functors) list, types: (string * typeConstrSet) list }) option;
end =

(*****************************************************************************)
(*                  COMPILERBODY functor body                                *)
(*****************************************************************************)
struct
  open MISC;
  open STRUCTVALS;
  open PRETTY;
  open LEX;

  type nameSpace = VALUEOPS.nameSpace
  open STRUCTURES
  open EXPORTTREE

    fun printTimes printOut (parseTime,pass2Time,treeTime,codeTime,endRunTime) : unit =
    let
        val message =
            PrettyBlock(3, false, [],
                [
                    PrettyString "Timing -",
                    PrettyBreak(1, 0),
                    PrettyString ("parse:" ^ Time.fmt 1 parseTime ^ ","),
                    PrettyBreak(1, 0),
                    PrettyString ("semantics:" ^ Time.fmt 1 (pass2Time-parseTime) ^ ","),
                    PrettyBreak(1, 0),
                    PrettyString ("translate:" ^ Time.fmt 1 (treeTime-pass2Time) ^ ","),
                    PrettyBreak(1, 0),
                    PrettyString ("generate:" ^ Time.fmt 1 (codeTime-treeTime) ^ ","),
                    PrettyBreak(1, 0),
                    PrettyString ("run:" ^ Time.fmt 1 endRunTime)
                ]
            )
    in
        printOut message
    end

  
  (* switch profiling on/off *)
  val systemProfile : int -> unit =
    RunCall.run_call1 RuntimeCalls.POLY_SYS_profiler;
  
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

  fun baseCompiler (lex : lexan, nameSpace: nameSpace, debugSwitches) : 
       exportTree option * (unit ->
           { fixes: (string * fixStatus) list, values: (string * values) list,
             structures: (string * structVals) list, signatures: (string * signatures) list,
             functors: (string * functors) list, types: (string * typeConstrSet) list }) option =
  let (* let1 *)

    val compilerOutput = getCompilerOutput debugSwitches
    
    val timing         = DEBUG.getParameter DEBUG.timingTag debugSwitches
    and printCodetree  = DEBUG.getParameter DEBUG.codetreeTag debugSwitches
    and printParsetree = DEBUG.getParameter DEBUG.parsetreeTag debugSwitches
    and profiling      = DEBUG.getParameter DEBUG.profilingTag debugSwitches

    val globals =
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

    (* Return the total user+system time since the start. *)
    fun cpuTime start =
    let
        val {usr, sys} = Timer.checkCPUTimer start
    in
        usr+sys
    end

    val startTime = Timer.startCPUTimer();
    
    val startLocn = location lex
    val () =
      if SYMSET.inside (sy lex, SYMSET.semicolon)
      then insymbol lex
      else ();
      
    val parentTreeNav =
        case List.find (Universal.tagIs rootTreeTag) debugSwitches of
            SOME opt => Universal.tagProject rootTreeTag opt
        |   NONE => { parent = NONE, next = NONE, previous = NONE }
  in (* let1 *)
    (* An empty declaration (or end of file!) *)
    if SYMSET.inside (sy lex, stopSyms)
    then if errorOccurred lex (* We could have, for example, an unterminated comment. *)
       then (NONE, NONE)
       else (SOME(locSpan(startLocn, location lex), []), 
                 SOME (fn () => { fixes=[], values=[], structures=[], functors=[], types=[], signatures=[] })
            ) (* Do nothing *)
    else let (* let2 *)
      (* create a "throw away" compiling environment for this topdec *)
      val newFixEnv = UTILITIES.searchList ();
      val enterFix  = #enter newFixEnv;
      val lookupFix = lookupDefault (#lookup newFixEnv) (#lookupFix globals);
   
      (* parse a program: a sequence of topdecs ending with a semicolon. *)
      val parseTree : STRUCTURES.program = 
        PARSEDEC.parseDec (stopSyms, lex, {enterFix  = enterFix, lookupFix = lookupFix});

      val parseTime = cpuTime startTime;

      val () = 
        if printParsetree
        then compilerOutput (STRUCTURES.displayProgram (parseTree, 10000))
        else ();
        
      in (* let2 *)
          if errorOccurred lex
          then (NONE, NONE) (* Error: No result and the parse tree won't be useful. *)
          else let (* let3 *)
          (* If no errors then do second pass to match identifiers
             and declarations and return type of expression. *)
          val () = STRUCTURES.pass2Structs (parseTree, lex, Env globals);
     
          val pass2Time = cpuTime startTime;
          
          in (* let3 *)
            if errorOccurred lex
            then (SOME(structsExportTree(parentTreeNav, parseTree)), NONE)
            else let (* let4 *)
              
              (* Only code-generate if there were no errors and
                 it's not a directive. *)
              val optimisedCode : CODETREE.codetree =
                STRUCTURES.gencodeStructs (parseTree, lex);
              val treeTime = cpuTime startTime;
              
              in
                if errorOccurred lex
                then (* Errors can be produced during the code-generation phase. *)
                   (SOME(structsExportTree(parentTreeNav, parseTree)), NONE) (* Error: No result. *)
                else let (* let5 *)
                       val () =
                         if printCodetree
                         then compilerOutput(CODETREE.pretty optimisedCode)
                         else ();
                      
                      val resultCode : unit -> CODETREE.codetree =
                         CODETREE.genCode(optimisedCode, debugSwitches);
                      val codeTime = cpuTime startTime;
                      
                      (* This is the procedure which is returned as the result of
                         the compilation. *)
                      fun executeCode() =
                        let
                            (* Save the value of the profiling switch,
                                 and switch profiling on if required. *)
                            val wasProfiling = profiling;
                    
                            val startRunTime = Timer.startCPUTimer(); (* Save the run-time *)
                            val () = startProfile wasProfiling;
            
                            val resultVec = (* Now run it *)
                                (* If we were profiling or timing we have to handle the exception to turn
                                   these off even though it will mess up information about where the exception
                                   was raised. *)
                                if wasProfiling <> 0 orelse timing
                                then
                                (
                                 resultCode ()
                                     handle exn => (* Exceptions in run-time code. *)
                                     let
                                         val () = stopProfile wasProfiling;
                                         val endRunTime = cpuTime startRunTime;
                                         (* if we are profiling, stop and force out the profile counts *)
                                         val () = stopProfile wasProfiling;
                                     in
                                         (* print out timing info *)
                                         if timing then printTimes compilerOutput (parseTime,pass2Time,treeTime,codeTime,endRunTime) else ();
                                         raise exn
                                     end
                                )
                                else resultCode ()
             
                            val () = stopProfile wasProfiling;
                            val endRunTime = cpuTime startRunTime;
                             
                            val extractedResults = STRUCTURES.pass4Structs (resultVec, parseTree)
            
                        in
                            (* print out timing info *)
                            if timing then printTimes compilerOutput (parseTime,pass2Time,treeTime,codeTime,endRunTime) else ();
                
                            extractedResults
                        end
                     
                    in
                        (SOME(structsExportTree (parentTreeNav, parseTree)), SOME executeCode)
                    end (* let5 *)
                end (* let4 *)
            end (* let3 *)
        end (* let2 *)
    end; (* let1 *)

    fun compiler (nameSpace: nameSpace, getChar: unit->char option, parameters: Universal.universal list) : 
       exportTree option * (unit ->
           { fixes: (string * fixStatus) list, values: (string * values) list,
             structures: (string * structVals) list, signatures: (string * signatures) list,
             functors: (string * functors) list, types: (string * typeConstrSet) list }) option =
    let
        val debugSwitches = parameters 
        val lex = LEX.initial(getChar, debugSwitches);
        val traceCompiler = DEBUG.getParameter DEBUG.traceCompilerTag debugSwitches

        val compilerOutput = getCompilerOutput parameters
        fun printReport s = compilerOutput(PrettyString s)
    in
    (
        if traceCompiler
        then PolyML.exception_trace (fn () => baseCompiler(lex, nameSpace, debugSwitches))
        else baseCompiler(lex, nameSpace, debugSwitches)
    )
      handle
        SML90.Interrupt =>
        (
          printReport ("Compilation interrupted\n");
          raise SML90.Interrupt
        )
         
      | InternalError s =>
        let
          val s' =
          "Exception- InternalError: " ^ String.toString s ^ " raised while compiling"
        in
          printReport (s' ^ "\n");
          raise Fail s'
        end
       
      | exn =>
        let
          val s' =
              "Exception- " ^ General.exnName exn ^ " unexpectedly raised while compiling"
        in
          printReport (s' ^ "\n");
          raise Fail s'
        end
    end

end; (* struct *)
