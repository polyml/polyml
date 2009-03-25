(*
    Title:      Final version of the PolyML structure
    Author:     David Matthews
    Copyright   David Matthews 2008-9

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
Based on:

    Title:      Poly Make Program.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)


(*
This is the version of the PolyML structure that can be compiled after we
have the rest of the basis library.  In particular it binds in TextIO.stdIn
and TextIO.stdOut.

This contains the top-level read-eval-print loop as well as "use" and
Poly/ML's "make".
*)
local
    open PolyML.NameSpace
    (*****************************************************************************)
    (*                  top-level name space                                     *)
    (*****************************************************************************)
    val globalTable = UniversalArray.univArray 10 (* Choose a number for the initial size. *)
    and tableMutex = Thread.Mutex.mutex() (* Lock to protect the table. *)
 
    local
        open Universal UniversalArray Thread.Thread Thread.Mutex
        (* Create universal tags for the name space. *)
        (* Should these be kept private here or included in the PolyML
           structure? *)
        val valTag: valueVal tag = tag()
        and fixTag: fixityVal tag = tag()
        and functorTag: functorVal tag = tag()
        and signatureTag: signatureVal tag = tag()
        and structureTag: structureVal tag = tag()
        and typeTag: typeVal tag = tag()
        
        (* Lock the mutex during any lookup or entry.  This is primarily to
           avoid the underlying hash table from being rehashed by different
           threads at the same time. *)
        fun protect mutx f = LibraryIOSupport.protect mutx f ()
        
        fun lookup t s = protect tableMutex (fn () => sub(globalTable, t, s));
        fun enter t (s,v) = protect tableMutex (fn () => update(globalTable, t, s, v));
        fun all t () = protect tableMutex (fn () => 
           fold (fn (s, v, l) => if tagIs t v then (s, tagProject t v)::l else l)
           [] globalTable)
        fun forget t tag s = protect tableMutex (fn () => delete(t, tag, s))
    in
        val globalNameSpace: PolyML.NameSpace.nameSpace =
            {
            lookupFix    = lookup fixTag,
            lookupSig    = lookup signatureTag,
            lookupVal    = lookup valTag,
            lookupType   = lookup typeTag,
            lookupFunct  = lookup functorTag,
            lookupStruct = lookup structureTag,
            enterFix     = enter fixTag,
            enterSig     = enter signatureTag,
            enterVal     = enter valTag,
            enterType    = enter typeTag,
            enterFunct   = enter functorTag,
            enterStruct  = enter structureTag,
            allFix       = all fixTag,
            allSig       = all signatureTag,
            allVal       = all valTag,
            allType      = all typeTag,
            allFunct     = all functorTag,
            allStruct    = all structureTag
            }

        val forgetFix    = forget globalTable fixTag
        and forgetSig    = forget globalTable signatureTag
        and forgetVal    = forget globalTable valTag
        and forgetType   = forget globalTable typeTag
        and forgetFunct  = forget globalTable functorTag
        and forgetStruct = forget globalTable structureTag
    end

    (* PolyML.compiler takes a list of these parameter values.  They all
       default so it's possible to pass only those that are actually
       needed. *)
    datatype compilerParameters =
        CPOutStream of string->unit
        (* Output stream for debugging and other output from the compiler.
           Provides a default stream for other output.  Default: TextIO.print *)
    |   CPNameSpace of PolyML.NameSpace.nameSpace
        (* Name space to look up and enter results.  Default: globalNameSpace *)
    |   CPErrorMessageProc of
            { message: PolyML.pretty, hard: bool, location: PolyML.location, context: PolyML.pretty option } -> unit
        (* Called by the compiler to generate error messages.
           Arguments (message, isHard, lineNo, context).  message is the message.
           isHard is true if this is an error, false if a warning. 
           location is the file-name, line number and position.  context is an
           optional extra piece of information showing the part of the parse tree
           where the error was detected.
           Default: print this to CPOutStream value using CPLineNo and CPFileName. *)
    |   CPLineNo of unit -> int
        (* Called by the compiler to get the current "line number".  This is passed
           to CPErrorMessageProc and the debugger.  It may actually be a more general
           location than a source line.  Default: fn () => 0 i.e. no line numbering. *)
    |   CPLineOffset of unit -> int
        (* Called by the compiler to get the current "offset".  This is passed
           to CPErrorMessageProc and the debugger.  This may either be an offset on
           the current file, a byte offset or simply zero.
           Default: fn () => 0 i.e. no line offset. *)
    |   CPFileName of string
        (* The current file being compiled.  This is used by the default CPErrorMessageProc
           and the debugger.  Default: "" i.e. interactive stream. *)
    |   CPPrintInAlphabeticalOrder of bool
        (* Whether to sort the results by alphabetical order before printing them.  Applies
           only to the default CPResultFun.  Default value of printInAlphabeticalOrder. *)
    |   CPPrintTypesWithStructure of bool
        (* Whether when printing the type of a value to include any structure name
           with the type constructors.  Default value of printTypesWithStructureName. *)
    |   CPResultFun of {
            fixes: (string * fixityVal) list, values: (string * valueVal) list,
            structures: (string * structureVal) list, signatures: (string * signatureVal) list,
            functors: (string * functorVal) list, types: (string * typeVal) list} -> unit
        (* Function to apply to the result of compiling and running the code.
           Default: print and enter the values into CPNameSpace. *)
    |   CPCompilerResultFun of
            PolyML.parseTree option *
            ( unit -> {
                fixes: (string * fixityVal) list, values: (string * valueVal) list,
                structures: (string * structureVal) list, signatures: (string * signatureVal) list,
                functors: (string * functorVal) list, types: (string * typeVal) list}) option -> unit -> unit
        (* Function to process the result of compilation.  This can be used to capture the
           parse tree even if type-checking fails.
           Default: Execute the code and call the result function if the compilation
           succeeds.  Raise an exception if the compilation failed. *)
    |   CPProfiling of int
        (* Control profiling.  0 is no profiling, 1 is time etc.  Default is value of PolyML.profiling. *)
    |   CPTiming of bool
        (* Control whether the compiler should time various phases of the
           compilation and also the run time. Default: value of PolyML.timing. *)
    |   CPDebug of bool
        (* Control whether calls to the debugger should be inserted into the compiled
           code.  This allows breakpoints to be set, values to be examined and printed
           and functions to be traced at the cost of a very large run-time overhead.
           Default: value of PolyML.Compiler.debug *)
    |   CPPrintDepth of unit->int
        (* This controls the depth of printing if the default CPResultFun is used.  It
           is also bound into any use of PolyML.print in the compiled code and will
           be called to get the print depth whenever that code is executed.
           Default: Get the current value of PolyML.print_depth. *)
    |   CPPrintStream of string->unit
        (* This is bound into any occurrence of PolyML.print and is used to produce
           the outut.  Default: CPOutStream. *)
    |   CPPrinterNameSpace of PolyML.NameSpace.nameSpace
        (* This is bound into any occurrence of PolyML.print, PolyML.makestring or
           General.exnMessage.  It is used to search for an exception identifier in
           order to print the argument of an exception packet.  It is also used to find
           infixed datatype constructors when printing values. e.g. it might print
           1::2 rather than ::(1,2) if lists weren't treated specially.
           Default: CPNameSpace *)
    |   CPErrorDepth of int
        (* Controls the depth of context to produce in error messages.
           Default : value of PolyML.error_depth. *)
    |   CPLineLength of int
        (* Bound into any occurrences of PolyML.print.  This is the length of a line
           used in the pretty printer.  Default: value of PolyML.line_length. *)

    (* References for control and debugging. *)
    val profiling = ref 0
    and timing = ref false
    and printDepth = ref 0
    and errorDepth = ref 6
    and lineLength = ref 77
    
    val assemblyCode = ref false
    and codetree = ref false
    and codetreeAfterOpt = ref false
    and pstackTrace = ref false
    and parsetree = ref false
    
    val debug = ref false
    val inlineFunctors = ref true
    val maxInlineSize = ref 40
    val printInAlphabeticalOrder = ref true
    val printTypesWithStructureName = ref true
    val traceCompiler = ref false
    
    val useMarkupInOutput = ref false
    
    fun prettyPrintWithMarkup(stream : string -> unit, lineWidth : int): PolyML.pretty -> unit =
    let
        open PolyML
        val openDeclaration = "\u001bD"
        val closeDeclaration = "\u001bd"
        val separator = "\u001b,"
        val finalSeparator = "\u001b;"
        
        fun beginMarkup context =
            case List.find (fn ContextLocation _ => true | _ => false) context of
                SOME (ContextLocation{file,startLine,startPosition,endLine,endPosition}) =>
                let
                    (* In the unlikely event there's an escape character in the
                       file name convert it to ESC-ESC. *)
                    fun escapeEscapes #"\u001b" = "\u001b\u001b"
                    |   escapeEscapes c = str c
                in
                    stream openDeclaration;
                    stream(String.translate escapeEscapes file);
                    stream separator;
                    stream(Int.toString startLine);
                    stream separator;
                    stream(Int.toString startPosition);
                    stream separator;
                    stream(Int.toString endPosition);
                    stream finalSeparator
                end
            |   _ => ()
            
        fun endMarkup context =
            List.app (fn ContextLocation _ => stream closeDeclaration | _ => ()) context

        val markup =
            if ! useMarkupInOutput
            then (beginMarkup, endMarkup)
            else (fn _ => (), fn _ => ())
    in
        prettyMarkup markup (stream, lineWidth)
    end;

    (* Top-level prompts. *)
    val prompt1 = ref "> " and prompt2 = ref "# ";
    
    (* If printTypesWithStructureName is set we transform the pretty
       printed structure by adding the structure name as prefix where
       appropriate. *)
    fun addStructurePrefix addPrefix p =
    let
        open PolyML
        (* The structure name is part of the block context. *)
        fun doAddPrefix prefix (PrettyBlock(depth, consistent, context, list)) =
        let
            (* Add together the structure prefixes. *)
            fun makePrefix context =
            let
                val newPrefix =
                    List.find (fn (ContextParentStructure _) => true | _ => false)
                        context
            in
                case newPrefix of
                    SOME (ContextParentStructure(sName, subContext)) =>
                        makePrefix subContext ^ sName ^ "."
                |   _  => prefix (* No (more) prefixes. *)
            end
            val thisPrefix = makePrefix context
        in
            (* Add this prefix to any strings within the block. *)
            PrettyBlock(depth, consistent, context, List.map (doAddPrefix thisPrefix) list)
        end
        
        |   doAddPrefix prefix (p as PrettyString s) =
                if prefix = "" then p else PrettyString(prefix ^ s)

        |   doAddPrefix _ p = p (* PrettyBreak *)
    in
        if addPrefix
        then doAddPrefix "" p
        else p (* Return unchanged. *)
    end

    
    (* Debugger control. *)

    (* Whenever we enter a function we push information onto this stack. *)
    type debugStackEntry =
    {
        lineNo: int,
        funName: string,
        fileName: string,
        space: PolyML.NameSpace.nameSpace,
        arguments: PolyML.NameSpace.valueVal
    }
    (* With the exception of the stack, which is thread-specific, all these are
       global variables and apply to any thread. Perhaps they should be thread-specific
       in which case the debugger will only be entered if the thread that set a breakpoint
       encounters it. *)
    local
        val stackTag: debugStackEntry list ref Universal.tag = Universal.tag()
    in
        (* Get the stack of previous calls.  Create a new one if necessary.*)
        fun getStack(): debugStackEntry list ref =
            case Thread.Thread.getLocal stackTag of
                NONE => let val stack = ref [] in Thread.Thread.setLocal(stackTag, stack); stack end
            |   SOME stack => stack
    end
    val debugLevel = ref 0
    (* Set to true to exit the debug loop.  Set by commands such as "continue". *)
    val exitLoop = ref false;
    (* Exception packet sent if this was continueWithEx. *)
    val debugExPacket: exn option ref = ref NONE
    (* Call tracing. *)
    val tracing = ref false;
    val breakNext = ref false;
    (* Single stepping. *)
    val stepDebug = ref false;
    val stepDepth = ref ~1; (* Only break at a stack size less than this. *)
    (* Break points.  We have three breakpoint lists: a list of file-line
       pairs, a list of function names and a list of exceptions. *)
    val lineBreakPoints = ref []
    and fnBreakPoints = ref []
    and exBreakPoints = ref []

    fun checkLineBreak (file, line) =
        let
            fun findBreak [] = false
             |  findBreak ((f, l) :: rest) =
                  (l = line andalso f = file) orelse findBreak rest
        in
            findBreak (! lineBreakPoints)
        end

    fun checkFnBreak exact name =
    let
        (* When matching a function name we allow match if the name
           we're looking for matches the last component of the name
           we have.  e.g. if we set a break for "f" we match F().S.f . *)
        fun matchName n =
            if name = n then true
            else if exact then false
            else
            let
                val nameLen = size name
                and nLen = size n
                fun isSeparator #"-" = true
                 |  isSeparator #")" = true
                 |  isSeparator #"." = true
                 |  isSeparator _    = false
            in
                nameLen > nLen andalso String.substring(name, nameLen - nLen, nLen) = n
                andalso isSeparator(String.sub(name, nameLen - nLen - 1))
            end
    in
        List.exists matchName (! fnBreakPoints)
    end

    (* Get the exception id from an exception packet.  The id is
       the first word in the packet.  It's a mutable so treat it
       as an int ref here. *)
    fun getExnId(ex: exn): int ref =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_load_word (ex, 0)
    
    fun checkExnBreak(ex: exn) =
        let val exnId = getExnId ex in List.exists (fn n => n = exnId) (! exBreakPoints) end

    fun printOut s =
        TextIO.print s
        (* If we get an exception while writing to stdOut we've got
           a big problem and can't continue.  It could happen if
           we have closed stdOut.  Try reporting the error through
           stdErr and exit. *)
        handle Thread.Thread.Interrupt => raise Thread.Thread.Interrupt
        |     exn =>
            (
                (
                    TextIO.output(TextIO.stdErr,
                        concat["Exception ", exnName exn,
                      	       " raised while writing to stdOut.\n"]);
                    TextIO.flushOut TextIO.stdErr (* probably unnecessary. *)
                ) handle _ => ();
                (* Get out without trying to do anything else. *)
                OS.Process.terminate OS.Process.failure
            )

    (* Try to print the appropriate line from the file.  Used in the debugger
       and debug functions. *)
    fun printSourceLine(fileName: string, line: int, funName: string) =
    let
        open TextIO
    in
        (* First just print where we are. *)
        if fileName = "" then () else printOut(concat[fileName, " "]);
        if line = 0 then () else printOut(concat[" line:", Int.toString line, " "]);
        printOut(concat["function:", funName, "\n"]);
        (* Try to print it.  This may fail if the file name was not a full path
           name and we're not in the correct directory. *)
        if fileName = "" then ()
        else
        let
            val fd = openIn fileName
            fun pLine n =
                case inputLine fd of
                    NONE => ()
                |   SOME s => if n = 1 then printOut s else pLine(n-1)
        in
            pLine line;
            closeIn fd
        end handle IO.Io _ => () (* If it failed simply ignore the error. *)
    end

    local
        open Bootstrap Bootstrap.Universal
        (* To allow for the possibility of changing the representation we don't make Universal
           be the same as Bootstrap.Universal. *)

        (* Default error message function. *)
        fun defaultErrorProc
            {message: PolyML.pretty, hard: bool, location={startLine=line, file, ...}, context: PolyML.pretty option} =
        let
            open PolyML
            val fullMessage =
                case context of
                    NONE => message
                |   SOME ctxt =>
                        PrettyBlock(0, true, [],
                            [ message, PrettyBreak(1, 0), PrettyString "Found near", PrettyBreak(1, 0), ctxt])
        in
            printOut(concat
               ( (if hard then ["Error-"] else ["Warning-"]) @
                 (if file = "" then [] else [" in '", file, "',"]) @
                 (if line = 0 then [] else [" line ", Int.toString line]) @
                 (if line = 0 andalso file = "" then [] else [".\n"])));
            prettyPrintWithMarkup(printOut, !lineLength) ((* Always add prefixes. *)addStructurePrefix true fullMessage)
        end

        (* Default function to print and enter a value. *)
        fun printAndEnter (inOrder: bool, space: PolyML.NameSpace.nameSpace,
                           stream: string->unit, depth: int, withStruct: bool)
            { fixes: (string * fixityVal) list, values: (string * valueVal) list,
              structures: (string * structureVal) list, signatures: (string * signatureVal) list,
              functors: (string * functorVal) list, types: (string * typeVal) list}: unit =
        let
            (* We need to merge the lists to sort them alphabetically. *)
            datatype decKind =
                FixStatusKind of fixityVal
            |   TypeConstrKind of typeVal
            |   SignatureKind of signatureVal
            |   StructureKind of structureVal
            |   FunctorKind of functorVal
            |   ValueKind of valueVal

            val decList =
                map (fn (s, f) => (s, FixStatusKind f)) fixes @
                map (fn (s, f) => (s, TypeConstrKind f)) types @
                map (fn (s, f) => (s, SignatureKind f)) signatures @
                map (fn (s, f) => (s, StructureKind f)) structures @
                map (fn (s, f) => (s, FunctorKind f)) functors @
                map (fn (s, f) => (s, ValueKind f)) values

            fun kindToInt(FixStatusKind _) = 0
            |   kindToInt(TypeConstrKind _) = 1
            |   kindToInt(SignatureKind _) = 2
            |   kindToInt(StructureKind _) = 3
            |   kindToInt(FunctorKind _) = 4
            |   kindToInt(ValueKind _) = 5

            fun order (s1: string, k1) (s2, k2) =
                    if s1 = s2 then kindToInt k1 <= kindToInt k2
                    else s1 <= s2

            fun quickSort (leq:'a -> 'a -> bool) ([]:'a list)      = []
            |   quickSort (leq:'a -> 'a -> bool) ([h]:'a list)     = [h]
            |   quickSort (leq:'a -> 'a -> bool) ((h::t) :'a list) =
            let
                val (after, befor) = List.partition (leq h) t
            in
                quickSort leq befor @ (h :: quickSort leq after)
            end;

            (* Don't sort the declarations if we want them in declaration order. *)
            val sortedDecs =
                if inOrder then quickSort order decList else decList

            fun printDec(n, FixStatusKind f) =
                (
                    if depth > 0
                    then prettyPrintWithMarkup (stream, !lineLength) (displayFix(n,f))
                    else ();
                    #enterFix space (n,f)
                )
            |   printDec(n, TypeConstrKind t) =
                (
                    if depth > 0
                    then prettyPrintWithMarkup (stream, !lineLength)
                        (addStructurePrefix withStruct (displayType(t, depth)))
                    else ();
                    #enterType space (n,t)
                )
            |   printDec(n, SignatureKind s) =
                (
                    if depth > 0
                    then prettyPrintWithMarkup (stream, !lineLength)
                        (addStructurePrefix withStruct (displaySig(s, depth, space)))
                    else ();
                    #enterSig space (n,s)
                )
            |   printDec(n, StructureKind s) =
                (
                    if depth > 0
                    then prettyPrintWithMarkup (stream, !lineLength)
                        (addStructurePrefix withStruct (displayStruct(s, depth, space)))
                    else ();
                    #enterStruct space (n,s)
                )
            |   printDec(n, FunctorKind f) =
                (
                    if depth > 0
                    then prettyPrintWithMarkup (stream, !lineLength)
                            (addStructurePrefix withStruct (displayFunct(f, depth, space)))
                    else ();
                    #enterFunct space (n,f)
                )
            |   printDec(n, ValueKind v) =
                (
                    if depth > 0
                    then prettyPrintWithMarkup (stream, !lineLength)
                            (addStructurePrefix withStruct (displayVal(v, depth, space)))
                    else ();
                    #enterVal space (n,v)
                )
        in
            List.app printDec sortedDecs
        end
    in
        fun polyCompiler (getChar: unit->char option, parameters: compilerParameters list) =
        let
            (* Find the first item that matches or return the default. *)
            fun find f def [] = def
              | find f def (hd::tl) =
                  case f hd of
                      SOME s => s
                  |   NONE => find f def tl
        
            val outstream = find (fn CPOutStream s => SOME s | _ => NONE) TextIO.print parameters
            val nameSpace = find (fn CPNameSpace n => SOME n | _ => NONE) globalNameSpace parameters
            val lineNo = find (fn CPLineNo l => SOME l | _ => NONE) (fn () => 0) parameters
            val lineOffset = find (fn CPLineOffset l => SOME l | _ => NONE) (fn () => 0) parameters
            val fileName = find (fn CPFileName s => SOME s | _ => NONE) "" parameters
            val printInOrder = find (fn CPPrintInAlphabeticalOrder t => SOME t | _ => NONE)
                                (! printInAlphabeticalOrder) parameters
            val profiling = find (fn CPProfiling i => SOME i | _ => NONE) (!profiling) parameters
            val timing = find  (fn CPTiming b => SOME b | _ => NONE) (!timing) parameters
            val printDepth = find (fn CPPrintDepth f => SOME f | _ => NONE) (fn () => !printDepth) parameters
            val printWithStruct = find (fn CPPrintTypesWithStructure t => SOME t | _ => NONE)
                                (! printTypesWithStructureName) parameters
            val resultFun = find (fn CPResultFun f => SOME f | _ => NONE)
               (printAndEnter(printInOrder, nameSpace, outstream, printDepth(), printWithStruct)) parameters
            val printString = find (fn CPPrintStream s => SOME s | _ => NONE) outstream parameters
            val printenv = find (fn CPPrinterNameSpace s => SOME s | _ => NONE) nameSpace parameters
            val errorProc =  find (fn CPErrorMessageProc f => SOME f | _ => NONE) defaultErrorProc parameters
            val debugging = find (fn CPDebug t => SOME t | _ => NONE) (! debug) parameters
            local
    			(* Default is to filter the parse tree argument. *)
    			fun defaultCompilerResultFun (_, NONE) = raise Fail "Static Errors"
                |   defaultCompilerResultFun (_, SOME code) = fn () => resultFun(code()) 
            in
                val compilerResultFun = find (fn CPCompilerResultFun f => SOME f | _ => NONE)
                    defaultCompilerResultFun parameters
            end

            (* TODO: Make this available as a parameter. *)
            val prettyOut = prettyPrintWithMarkup(printString, !lineLength)
            
            val compilerOut = prettyPrintWithMarkup(outstream, !lineLength)


            (* Pass all the settings.  Some of these aren't included in the parameters datatype (yet?). *)
            val treeAndCode =
                PolyML.compiler(nameSpace, getChar,
                    [
                    tagInject errorMessageProcTag errorProc,
                    tagInject compilerOutputTag compilerOut,
                    tagInject lineNumberTag lineNo,
                    tagInject offsetTag lineOffset,
                    tagInject fileNameTag fileName,
                    tagInject inlineFunctorsTag (! inlineFunctors),
                    tagInject maxInlineSizeTag (! maxInlineSize),
                    tagInject parsetreeTag (! parsetree),
                    tagInject codetreeTag (! codetree),
                    tagInject pstackTraceTag (! pstackTrace),
                    tagInject assemblyCodeTag (! assemblyCode),
                    tagInject codetreeAfterOptTag (! codetreeAfterOpt),
                    tagInject timingTag timing,
                    tagInject profilingTag profiling,
                    tagInject errorDepthTag (! errorDepth),
                    tagInject printDepthFunTag printDepth,
                    tagInject lineLengthTag (! lineLength),
                    tagInject traceCompilerTag (! traceCompiler),
                    tagInject debugTag debugging,
                    tagInject printEnvironTag printenv,
                    tagInject debuggerTag debugFunction,
                    tagInject printOutputTag prettyOut
                    ])
        in
		    compilerResultFun treeAndCode
        end
 
        (* Top-level read-eval-print loop.  This is the normal top-level loop but is
           also used for the debugger so has to be mutually recursively defined with it. *)
        and topLevel isDebug (nameSpace, exitLoop) : unit =
        let
            (* This is used as the main read-eval-print loop.  It is also invoked
               by running code that has been compiled with the debug option on
               when it stops at a breakpoint.  In that case debugEnv contains an
               environment formed from the local variables.  This is placed in front
               of the normal top-level environment. *)
           
            (* Don't use the end_of_stream because it may have been set by typing
               EOT to the command we were running. *)
            val endOfFile    = ref false;
            val realDataRead = ref false;
            val lastWasEol   = ref true;
    
            (* Each character typed is fed into the compiler but leading
               blank lines result in the prompt remaining as firstPrompt until
               significant characters are typed. *)
            fun readin () : char option =
            let
                val setPrompt : unit =
                    if !lastWasEol (* Start of line *)
                    then if !realDataRead
                    then printOut (if isDebug then "debug " ^ !prompt2 else !prompt2)
                    else printOut (if isDebug then "debug " ^ !prompt1 else !prompt1)
                    else ();
             in
                case TextIO.input1 TextIO.stdIn of
                    NONE => (endOfFile := true; NONE)
                |   SOME #"\n" => ( lastWasEol := true; SOME #"\n" )
                |   SOME ch =>
                       (
                           lastWasEol := false;
                           if ch <> #" "
                           then realDataRead := true
                           else ();
                           SOME ch
                       )
            end; (* readin *)
    
            (* Remove all buffered but unread input. *)
            fun flushInput () =
                case TextIO.canInput(TextIO.stdIn, 1) of
                    SOME 1 => (TextIO.inputN(TextIO.stdIn, 1); flushInput())
                |   _ => (* No input waiting or we're at EOF. *) ()
    
            fun readEvalPrint () : unit =
            let
                (* If we have executed a deeply recursive function the stack will have
                   extended to be very large.  It's better to reduce the stack if we
                   can.  This is RISKY.  Each function checks on entry that the stack has
                   sufficient space for everything it will allocate and assumes the stack
                   will not shrink.  It's unlikely that any of the functions here will
                   have asked for very much but as a precaution we allow for an extra 8k words. *)
                fun shrink_stack (newsize : int) : unit = 
                    RunCall.run_call1 RuntimeCalls.POLY_SYS_shrink_stack newsize
                val () = if isDebug then () else shrink_stack 8000;
            in
                realDataRead := false;
                (* Compile and then run the code. *)
                let
                    val code =
                        polyCompiler(readin, [CPNameSpace nameSpace, CPOutStream printOut])
                        handle Fail s => 
                        (
                            printOut(s ^ "\n");
                            flushInput();
                            lastWasEol := true;
                            raise Fail s
                        )
                in
                    code ()
                    (* Report exceptions in running code. *)
                        handle  exn =>
                        (
                            printOut ("Exception- " ^ PolyML.makestringInNameSpace(exn, globalNameSpace) ^ " raised\n");
                            raise exn
                        )
                end
            end; (* readEvalPrint *)
            
            fun handledLoop () : unit =
            (
                (* Process a single top-level command. *)
                readEvalPrint() handle _ => ();
                (* Exit if we've seen end-of-file or we're in the debugger
                   and we've run "continue". *)
                if !endOfFile orelse exitLoop() then ()
                else handledLoop ()
            )
        in
            handledLoop ()  
        end

        (* Debug function.  Calls to this function are inserted in the compiled code
           if the code is compiled with debugging on. *)
        and debugFunction(code, value, line, file, name, debugEnv) =
        let
            val stack: debugStackEntry list ref = getStack()
            fun printVal v =
                prettyPrintWithMarkup(TextIO.print, 77) (Bootstrap.printValue(v, !printDepth, globalNameSpace))

            fun enterDebugger ()=
            let
                (* Remove any type-ahead. *)
                fun flushInput () =
                    case TextIO.canInput(TextIO.stdIn, 1) of
                        SOME 1 => (TextIO.inputN(TextIO.stdIn, 1); flushInput())
                    |   _ => ()
                val () = flushInput ()

                val () = exitLoop := false;
                val () = debugLevel := 0;
                val () = breakNext := false;
                val () =
                    case !stack of
                        {lineNo, fileName, funName, ...} :: _ => printSourceLine(fileName, line, funName)
                    |   [] => () (* Shouldn't happen. *)

                val compositeNameSpace = (* Compose any debugEnv with the global environment *)
                let
                    (* The debugging environment depends on the currently selected stack frame. *)
                    fun debugEnv() = #space (List.nth(!stack, !debugLevel))
                    fun dolookup f s = case f (debugEnv()) s of NONE => f globalNameSpace s | v => v
                    fun getAll f () = f (debugEnv()) () @ f globalNameSpace ()
                in
                    {
                    lookupFix    = dolookup #lookupFix,
                    lookupSig    = dolookup #lookupSig,
                    lookupVal    = dolookup #lookupVal,
                    lookupType   = dolookup #lookupType,
                    lookupFunct  = dolookup #lookupFunct,
                    lookupStruct = dolookup #lookupStruct,
                    enterFix     = #enterFix globalNameSpace,
                    enterSig     = #enterSig globalNameSpace,
                    enterVal     = #enterVal globalNameSpace,
                    enterType    = #enterType globalNameSpace,
                    enterFunct   = #enterFunct globalNameSpace,
                    enterStruct  = #enterStruct globalNameSpace,
                    allFix       = getAll #allFix,
                    allSig       = getAll #allSig,
                    allVal       = getAll #allVal,
                    allType      = getAll #allType,
                    allFunct     = getAll #allFunct,
                    allStruct    = getAll #allStruct
                    }
                end
            in
                topLevel true (compositeNameSpace, fn _ => ! exitLoop);

                (* If this was continueWithEx raise the exception. *)
                case ! debugExPacket of
                    NONE => ()
                |   SOME exn => (debugExPacket := NONE; raise exn)
            end

            fun printSpaces () =
            let
                fun printSp 0 = () | printSp n = (print " "; printSp (n-1))
                val depth = List.length(! stack)
            in
                if depth > 50
                then printSp 50
                else if depth = 0
                then ()
                else printSp (depth-1)
            end
                
         in
            case code of
                1 => (* Entry to function *)
                let
                    (* Push this onto the stack. *)
                    val newStackEntry: debugStackEntry =
                        { lineNo = line, funName = name, fileName = file,
                          space = debugEnv, arguments = value}
                in
                    stack := newStackEntry :: !stack;
                    if ! tracing
                    then (printSpaces(); print name; print " "; printVal value; print "\n")
                    else ();
                    (* We don't actually break here because at this stage we don't
                       have any variables declared. *)
                    if checkLineBreak (name, line) orelse checkFnBreak false name
                    then breakNext := true
                    else ()
                end

            |   2 => (* Return from function. *)
                let
                     val (args, stackTail) =
                        case !stack of
                            [] => (value, []) (* Use the passed in value for the arg. *)
                        |   {arguments, ...} ::tl => (arguments, tl)
                in
                    if ! tracing
                    then (printSpaces(); print name; print " "; printVal args; print " = "; printVal value; print "\n")
                    else ();
                    (* Pop the stack. *)
                    stack := stackTail
                end

            |   3 => (* Function raised an exception. *)
                let
                     val (args, stackTail) =
                        case !stack of
                            [] => (value, []) (* Use the passed in value for the arg. *)
                        |   {arguments, ...} ::tl => (arguments, tl)
                in
                    if ! tracing
                    then (printSpaces(); print name; print " "; printVal args; print " raised "; printVal value; print "\n")
                    else ();
                    if checkExnBreak(Bootstrap.getValue value)
                    then enterDebugger ()
                    else ();
                    (* Pop the stack. *)
                    stack := (case !stack of [] => [] | _::tl => tl)
                end

            |   4 => (* Change of line within a function *)
                let
                    val (args, stackTail) =
                        (* If this is top-level code the stack may be empty. *)
                        case !stack of
                            [] => (value, []) (* Use the passed in value for the arg. *)
                        |   {arguments, ...} ::tl => (arguments, tl);

                    (* Update the entry but include the original arguments. *)
                    val newStackEntry: debugStackEntry =
                        { lineNo = line, funName = name, fileName = file,
                          space = debugEnv, arguments = args}
                in
                    (* Update the stack.  If this is top-level code the stack may be empty. *)
                    stack := newStackEntry :: stackTail;
                    (* We need to enter the debugger if we are single stepping or
                       we have a break at this line or we've just entered a function with a
                       break point. *)
                    if (!stepDebug andalso (!stepDepth < 0 orelse List.length(!stack) <= !stepDepth)) orelse
                       checkLineBreak (name, line) orelse ! breakNext
                    then enterDebugger ()
                    else () 
                end
            |   _ => ()
        end

        (* Normal, non-debugging top-level loop. *)
        fun shell addMarkup =
            (useMarkupInOutput := addMarkup; topLevel false (globalNameSpace, fn _ => false))

    end


    val bindName = ref "ml_bind";
    val archSuffix = "." ^ String.map Char.toLower (PolyML.architecture())
    (* The architecture-specific suffixes take precedence. *)
    val suffixes = ref [archSuffix, "",archSuffix^".ML", ".ML", archSuffix^".sml", ".sml"];

    (* isDir raises an exception if the file does not exist so this is
       an easy way to test for the file. *)
    fun fileDirExists (name : string) : bool =
       (OS.FileSys.isDir name; true) handle OS.SysErr _ => false

    fun findFileTuple (directory, object) [] = NONE
    |   findFileTuple (directory, object) (suffix :: suffixes) =
    let
        val fileName  = object ^ suffix
    in
        if fileDirExists (OS.Path.joinDirFile{dir=directory, file = fileName})
        then SOME (directory, fileName)
        else findFileTuple (directory, object) suffixes
    end;

    (* If we are building under the IDE we need to record the dependencies
       and also save the state before each file we "use". *)
    val saveDirectory: string option ref = ref NONE
    val dependencies: string list ref = ref []
    
    fun preUse fileName =
    case saveDirectory of
        ref NONE => ()
    |   ref (SOME dirName) =>
        (let
            (* Create a directory hierarchy. *)
            fun createDirs path =
                if path = "" orelse (OS.FileSys.isDir path handle OS.SysErr _ => false)
                then ()
                else
                (
                    createDirs (OS.Path.dir path);
                    OS.FileSys.mkDir path
                );

            local
                open OS.FileSys OS.Path
                val baseName = joinDirFile { dir = dirName, file = fileName }
            in
                val saveFile =
                    mkCanonical (joinBaseExt{ base = baseName, ext = SOME "save"})
                val depFile =
                    mkCanonical (joinBaseExt{ base = baseName, ext = SOME "deps"})
            end
            (* Reset the save directory before we save so that it isn't set in the saved
               state.  That means that "use" won't save the state unless it's explicitly
               asked to. *)
            val saveSave = ! saveDirectory
       in
            (* Create any containing directories. *)
            createDirs(OS.Path.dir saveFile);
            saveDirectory := NONE;
            
            (* Save the state. *)
            PolyML.SaveState.saveChild (saveFile, List.length(PolyML.SaveState.showHierarchy()));
            (* Restore the ref. *)
            saveDirectory := saveSave;
            
            (* Write out the dependencies. *)
            let
                open TextIO
                val f = openOut depFile
            in
                List.app(fn s => output(f, s ^ "\n")) (!dependencies);
                closeOut f
            end;
            (* Add this file to the dependency list. *)
            dependencies := ! dependencies @ [fileName]
        end handle (ex as OS.SysErr args) =>
                    (print (concat["Exception SysErr(", PolyML.makestring args, ") raised for ", fileName, "\n"]); raise ex))

    (*****************************************************************************)
    (*                  "use": compile from a file.                              *)
    (*****************************************************************************)

    fun use originalName =
    let
        (* use "f" first tries to open "f" but if that fails it tries "f.ML", "f.sml" etc. *)
        fun trySuffixes [] =
            (* Not found - attempt to open the original and pass back the
               exception. *)
            (TextIO.openIn originalName, originalName)
         |  trySuffixes (s::l) =
            (TextIO.openIn (originalName ^ s), originalName ^ s)
                handle IO.Io _ => trySuffixes l
        (* First in list is the name with no suffix. *)
        val (inStream, fileName) = trySuffixes("" :: ! suffixes)
        
        val () = preUse fileName

        val lineNo   = ref 1;
        fun getChar () : char option =
            case TextIO.input1 inStream of
                eoln as SOME #"\n" =>
                (
                    lineNo := !lineNo + 1;
                    eoln
                )
            |   c => c
    in
        while not (TextIO.endOfStream inStream) do
        let
            val code = polyCompiler(getChar, [CPFileName fileName, CPLineNo(fn () => !lineNo)])
                handle exn => (TextIO.closeIn inStream; raise exn)
        in
            code() handle exn =>
            (
                (* Report exceptions in running code. *)
                TextIO.print ("Exception- " ^ PolyML.makestringInNameSpace(exn, globalNameSpace) ^ " raised\n");
                TextIO.closeIn inStream;
                raise exn
            )
        end;
        (* Normal termination: close the stream. *)
        TextIO.closeIn inStream

    end (* use *)
 
    fun maxTime (x : Time.time, y : Time.time) : Time.time = 
        if x < y then y else x;

    exception ObjNotFile;
    
    type 'a tag = 'a Universal.tag;
  
    fun splitFilename (name: string) : string * string =
       let
         val {dir, file } = OS.Path.splitDirFile name
     in
         (dir, file)
     end

    (* Make *)
    (* There are three possible states - The object may have been checked,
     it may be currently being compiled, or it may not have been
     processed yet. *)
    datatype compileState = NotProcessed | Searching | Checked;

    fun longName (directory, file) = OS.Path.joinDirFile{dir=directory, file = file}
    
    fun fileReadable (fileTuple as (directory, object)) =
        (* Use OS.FileSys.isDir just to test if the file/directory exists. *)
        if (OS.FileSys.isDir (longName fileTuple); false) handle _ => true
        then false
        else
        let
            (* Check that the object is present in the directory with the name
             given and not a case-insensitive version of it.  This avoids
             problems with "make" attempting to recursively make Array etc
             because they contain signatures ARRAY. *)
            open OS.FileSys
            val d = openDir (if directory = "" then "." else directory)
            fun searchDir () =
              case readDir d of
                 NONE => false
              |  SOME f => f = object orelse searchDir ()
            val present = searchDir()
        in
            closeDir d;
            present
        end
    
    fun findFileTuple (directory, object) [] = NONE
    |   findFileTuple (directory, object) (suffix :: suffixes) =
    let
        val fileName  = object ^ suffix
        val fileTuple = (directory, fileName)
    in
        if fileReadable fileTuple
        then SOME fileTuple
        else findFileTuple (directory, object) suffixes
    end;
    
    fun filePresent (directory : string, object : string) =
        findFileTuple (directory, object) (!suffixes)
    
    (* See if the corresponding file is there and if it is a directory. *)
    fun testForDirectory (name: string) : bool =
        OS.FileSys.isDir name handle _ => false (* No such file. *)

    (* Time stamps. *)
    type timeStamp = Time.time;
    val firstTimeStamp : timeStamp = Time.zeroTime;
    (* Get the current time. *)
    val newTimeStamp : unit -> timeStamp = Time.now
    (* Get the date of a file. *)
    val fileTimeStamp : string -> timeStamp = OS.FileSys.modTime
    (* String representation - includes trailing "\n"! *)
    fun stringOfTimeStamp (t : timeStamp) : string =
        Date.toString(Date.fromTimeLocal t) ^ "\n"
    
    local
        open Universal
    in
        val timeStampTagMethods    : timeStamp tag   = tag ();
        val dependenciesTagMethods : string list tag = tag ();
    end;

    fun lastMade (objectName : string) : timeStamp =
        getOpt(UniversalArray.sub(globalTable, timeStampTagMethods, objectName), firstTimeStamp);

    (* Main make function *)
    fun make (targetName: string) : unit =
    let
        (* This serves two purposes. It provides a list of objects which have been
           re-made to prevent them being made more than once, and it also prevents
           circular dependencies from causing infinite loops (e.g. let x = f(x)) *)
            local
                open HashArray;
                val htab : compileState hash = hash 10;
            in
                fun lookupStatus (name: string) : compileState =
                    getOpt(sub (htab, name), NotProcessed);
                  
                fun setStatus (name: string, cs: compileState) : unit =
                    update (htab, name, cs)
            end;

        (* Remove leading directory names to get the name of the object itself.
           e.g. "../compiler/parsetree/gencode" yields simply "gencode". *)
        val (dirName,objectName) = splitFilename targetName;
 
        (* Looks to see if the file is in the current directory. If it is and
           the file is newer than the corresponding object then it must be
           remade. If it is a directory then we attempt to remake the
           directory by compiling the "bind" file. This will only actually be
           executed if it involves some identifier which is newer than the
           result object. *)
        fun remakeObj (objName: string) (findDirectory: string -> string) =
        let
        (* Find a directory that contains this object. An exception will be
             raised if it is not there. *)
            val directory = findDirectory objName;
            val fullName  =
                if directory = "" (* Work around for bug. *)
                then objName
                else OS.Path.joinDirFile{dir=directory, file=objName};

            val objIsDir  = testForDirectory fullName;
            val here      = fullName;
      
            (* Look to see if the file exists, possibly with an extension,
               and get the extended version. *)
            val fileTuple =
                let
                    (* If the object is a directory the source is in the bind file. *)
                    val (dir : string, file : string) =
                        if objIsDir
                        then (here, !bindName)
                        else (directory, objName);
                in
                    case filePresent (dir, file) of
                        SOME res' => res'
                    |   NONE      => raise Fail ("No such file or directory ("^file^","^dir^")")
                end ;
            
            val fileName = longName fileTuple;

            val newFindDirectory : string -> string =
                if objIsDir
                then
                let
                    (* Look in this directory then in the ones above. *)
                    fun findDirectoryHere (name: string) : string =
                        case filePresent (here, name) of
                          NONE => findDirectory name (* not in this directory *)
                        | _    => here;
                in
                    findDirectoryHere
                end
                else findDirectory;
    
            (* Compiles a file. *)
            fun remakeCurrentObj () =
            let
                val () = print ("Making " ^ objName ^ "\n");
                local
                    (* Keep a list of the dependencies. *)
                    val deps : bool HashArray.hash = HashArray.hash 10;
                    
                    fun addDep name =
                        if getOpt(HashArray.sub (deps, name), true)
                        then HashArray.update(deps, name, true)
                        else ();
                    
                    (* Called by the compiler to look-up a global identifier. *)
                    fun lookupMakeEnv globalLook (name: string) : 'a option =
                    let
                        (* Have we re-declared it ? *)
                        val res = lookupStatus name;
                    in
                        case res of
                            NotProcessed  =>
                            (
                                (* Compile the dependency. *)
                                remakeObj name newFindDirectory;
                                (* Add this to the dependencies. *)
                                addDep name
                            )

                        |  Searching => (* In the process of making it *)
                           print("Circular dependency: " ^ name ^  " depends on itself\n")

                        | Checked => addDep name; (* Add this to the dependencies. *)

                        (* There was previously a comment about returning NONE here if
                           we had a problem remaking a dependency. *)
                        globalLook name
                    end; (* lookupMakeEnv *)

                     (* Enter the declared value in the table. Usually this will be the
                        target we are making. Also set the state to "Checked". The
                        state is set to checked when we finish making the object but
                        setting it now suppresses messages about circular dependencies
                        if we use the identifier within the file. *)
                    fun enterMakeEnv (kind : string, enterGlobal) (name: string, v: 'a) : unit =
                    (
                        (* Put in the value. *)
                        enterGlobal (name, v);
                        print ("Created " ^ kind ^ " " ^ name ^ "\n");
                        
                        (* The name we're declaring may appear to be a dependency
                           but isn't, so don't include it in the list. *)
                        HashArray.update (deps, name, false);
                        
                        if name = objName
                        then
                        let
                            (* Put in the dependencies i.e. those names set to true in the table. *)
                            val depends =
                                HashArray.fold (fn (s, v, l) => if v then s :: l else l) [] deps;
                            
                            (* Put in a time stamp for the new object.  We need to make
                               sure that it is no older than the newest object it depends
                               on.  In theory that should not be a problem but clocks on
                               different machines can get out of step leading to objects
                               made later having earlier time stamps. *)
                            val newest =
                                List.foldl (fn (s: string, t: timeStamp) =>
                                    maxTime (lastMade s, t)) (fileTimeStamp fileName) depends;
                            
                            val timeStamp = maxTime(newest, newTimeStamp());
                        in         
                            setStatus (name, Checked);
                            UniversalArray.update(globalTable, dependenciesTagMethods, name, depends);
                            UniversalArray.update(globalTable, timeStampTagMethods, name, timeStamp)
                        end
                        else ()
                    ) (* enterMakeEnv *);
     
                in
                    val makeEnv =
                        { 
                            lookupFix    = #lookupFix globalNameSpace,
                            lookupVal    = #lookupVal globalNameSpace,
                            lookupType   = #lookupType globalNameSpace,
                            lookupSig    = lookupMakeEnv (#lookupSig globalNameSpace),
                            lookupStruct = lookupMakeEnv (#lookupStruct globalNameSpace),
                            lookupFunct  = lookupMakeEnv (#lookupFunct globalNameSpace),
                            enterFix     = #enterFix globalNameSpace,
                            enterVal     = #enterVal globalNameSpace,
                            enterType    = #enterType globalNameSpace,
                            enterStruct  = enterMakeEnv ("signature", #enterStruct globalNameSpace),
                            enterSig     = enterMakeEnv ("signature", #enterSig globalNameSpace),
                            enterFunct   = enterMakeEnv ("signature", #enterFunct globalNameSpace),
                            allFix       = #allFix globalNameSpace,
                            allVal       = #allVal globalNameSpace,
                            allType      = #allType globalNameSpace,
                            allSig       = #allSig globalNameSpace,
                            allStruct    = #allStruct globalNameSpace,
                            allFunct     = #allFunct globalNameSpace
                        };
                end; (* local for makeEnv *)

                val inputFile = OS.Path.joinDirFile{dir= #1 fileTuple, file= #2 fileTuple}
    
                val inStream = TextIO.openIn inputFile;
    
                val () =
                let (* scope of exception handler to close inStream *)
                    val endOfStream = ref false;
                    val lineNo     = ref 1;
        
                    fun getChar () : char option =
                        case TextIO.input1 inStream of
                            NONE => (endOfStream := true; NONE) (* End of file *)
                        |   eoln as SOME #"\n" => (lineNo := !lineNo + 1; eoln)
                        |   c => c
                 in
                    while not (!endOfStream) do
                    let
                        val code = polyCompiler(getChar,
                            [CPNameSpace makeEnv, CPFileName fileName, CPLineNo(fn () => !lineNo)])
                    in
                        code ()
                            handle exn as Fail _ => raise exn
                            |  exn =>
                            (
                                print ("Exception- " ^ PolyML.makestringInNameSpace(exn, globalNameSpace) ^ " raised\n");
                                raise exn
                            )
                    end
                end (* body of scope of inStream *)
                    handle exn => (* close inStream if an error occurs *)
                    (
                        TextIO.closeIn inStream;
                        raise exn
                    )
            in (* remake normal termination *)
                TextIO.closeIn inStream;
                (* For "use" we save the state before the "use" but for "make" we have
                   to save the state after we have found any dependencies.  That means
                   that a saved state for a file will contain declarations for the file
                   itself. *)
                preUse inputFile
            end (* remakeCurrentObj *)
            
        in (* body of remakeObj *)
            setStatus (objName, Searching);
         
             (* If the file is newer than the object then we definitely must remake it.
               Otherwise we look at the dependency list and check those. If the result
               of that check is that one of the dependencies is newer than the object
               (probably because it has just been recompiled) we have to recompile
               the file. Compiling a file also checks the dependencies and recompiles
               them, generating a new dependency list. That is why we don't check the
               dependency list if the object is out of date with the file. Also if the
               file has been changed it may no longer depend on the things it used to
               depend on. *)
 
            let
                val objDate = lastMade objName
       
                fun maybeRemake (s:string) : unit =
                case lookupStatus s of
                    NotProcessed => (* see if it's a file. *)
                        (* Compile the dependency. *)
                        remakeObj s newFindDirectory
                    
                    | Searching => (* In the process of making it *)
                        print ("Circular dependency: " ^ s ^ " depends on itself\n")
                    
                    |  Checked => () (* do nothing *)
                    
                
                (* Process each entry and return true if
                   any is newer than the target. *)
                val processChildren =
                    List.foldl
                    (fn (child:string, parentNeedsMake:bool) =>
                        (
                            maybeRemake child;
                            (* Find its date and see if it is newer. *)
                            parentNeedsMake orelse lastMade child > objDate
                        )
                    )
                    false;
            in
                if objDate < fileTimeStamp fileName orelse
                    (
                        (* Get the dependency list. There may not be one if
                           this object has not been compiled with "make". *) 
                        case UniversalArray.sub(globalTable, dependenciesTagMethods, objName) of
                            SOME d => processChildren d
                        |   NONE => true (* No dependency list - must use "make" on it. *)
                    )       
                then remakeCurrentObj ()
                else ()
            end;

            (* Mark it as having been checked. *)
            setStatus (objName, Checked)
        end (* body of remakeObj *)
  
        (* If the object is not a file mark it is checked. It may be a
           pervasive or it may be missing. In either case mark it as checked
           to save searching for it again. *)
        handle
                ObjNotFile => setStatus (objName, Checked)
            
            |   exn => (* Compilation (or execution) error. *)
                (
                    (* Mark as checked to prevent spurious messages. *)
                    setStatus (objName, Checked);
                    raise exn
                )
    in (*  body of make *)
        (* Check that the target exists. *)
        case filePresent (dirName, objectName) of
            NONE =>
            let
                val dir =
                    if dirName = "" then ""
                    else " (directory "^dirName^")";
                val s = "File "^objectName^" not found" ^ dir
            in
                print (s ^ "\n");
                raise Fail s
            end
        
        | _ =>
        let
            val targetIsDir = testForDirectory targetName;
            
            (* If the target we are making is a directory all the objects
               must be in the directory. If it is a file we allow references
               to other objects in the same directory. Objects not found must
               be pervasive. *)
            fun findDirectory (s: string) : string =
                if (not targetIsDir orelse s = objectName) andalso
                    filePresent(dirName,  s) <> NONE
                then dirName
                else raise ObjNotFile;
        in
            remakeObj objectName findDirectory
                handle exn  => 
                (
                    print (targetName ^ " was not declared\n");
                    raise exn
                )
        end
    end (* make *)

    (* This is the root function to run the Poly/ML top level. *)
    fun rootShell () =
    let
        val argList = CommandLine.arguments();
        fun rtsRelease() = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (10, ())
        fun rtsCopyright() = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (11, ())
        fun rtsHelp() = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (19, ())
    in
        if List.exists(fn s => s = "-v") argList
        then (* -v option : Print version information and exit *)
            print (String.concat ["Poly/ML ", Bootstrap.compilerVersion, 
                                 "    RTS version: ", rtsRelease(), "\n"])

        else if List.exists(fn s => s = "--help") argList
        then (* --help option: Print argument information and exit. *)
           (
            print (String.concat ["Poly/ML ", Bootstrap.compilerVersion, "\n"]);
            print "Compiler arguments:\n";
            print "\n";
            print "-v             Print the version of Poly/ML and exit\n";
            print "--help         Print this message and exit\n";
            print "-q             Suppress the start-up message\n";
            print "--with-markup  Include extra mark-up information when printing\n";
            print "--ideprotocol  Run the IDE communications protocol\n";
            print "\nRun time system arguments:\n";
            print (rtsHelp())
           )
           
        else if List.exists(fn s => s = "--ideprotocol") argList
        then (* Run the IDE communication protocol. *)
        let 
            val () = useMarkupInOutput := true;

            (* Parse trees for topdecs in current file. *)
            val parseTrees = ref []
            (* Save the last parsetree here. *)
            val lastParsetree = ref NONE

            (* Print the beginning of a response packet. *)
            fun printLocation startCh =
            let
                val (start, End) =
                    case lastParsetree of
                        ref NONE => (0, 0)
                    |   ref (SOME ({startPosition, endPosition, ...}, _)) =>
                            (startPosition, endPosition)
            in
                print (String.concat["\u001b", String.str startCh,
                    Int.toString start, "\u001b,", Int.toString End])
            end
 
            datatype direction = Up | Down | Left | Right
            
            fun toplevel () =
            case ! parseTrees of
                [] => raise Fail "Empty Tree"
            |   trees as (hd :: _) =>
                let
                    open PolyML
                    (* Navigation for one or more topdecs. *)
                    val fullLoc =
                        case (hd, List.last trees) of
                            (({ file, startLine, startPosition, ... }, _),
                             ({ endLine, endPosition, ... }, _)) =>
                            {
                                file=file, startLine=startLine,
                                startPosition=startPosition,
                                endLine=endLine, endPosition=endPosition
                            }
                    fun makelist([], _) = (* Shouldn't happen *) raise Fail "Null list"
                    |   makelist(l as (locn, props) :: tl, previous) =
                        let
                            fun this () = makelist(l, previous)
                            (* If there is another item in the list we need a
                               property that moves there whose "previous" property
                               comes here. *)
                            val next =
                                case tl of
                                    [] => []
                                |   n => [PTnextSibling(
                                            fn () => makelist(tl, [PTpreviousSibling this]))]
                        in
                            (locn, previous @ next @ props)
                        end
                in
                    (fullLoc, [PTfirstChild(fn () => makelist(trees, []))])
                end
            

            (* Move in the selected direction. *)
            fun navigate dir =
            case lastParsetree of
                ref NONE => ()
            |   ref (SOME(location, tree)) =>
                let
                    open PolyML
                    fun find([], _) = (location, tree) (* No change *)
                    |   find(PTparent p :: _, Up) = p()
                    |   find(PTpreviousSibling p :: _, Left) = p()
                    |   find(PTnextSibling p :: _, Right) = p()
                    |   find(PTfirstChild p :: _, Down) = p()
                    |   find(_ :: tl, dir) = find (tl, dir)
                    
                in
                    lastParsetree := SOME(find(tree, dir))
                end
            
            fun navigateTo(startLocn, endLocn) =
            case lastParsetree of
                ref NONE => ()
            |   ref (SOME(location as { startPosition, endPosition, ... }, tree)) =>
                let
                    open PolyML
                    fun find([], _) = NONE (* No change *)
                    |   find(PTparent p :: _, Up) = SOME p
                    |   find(PTpreviousSibling p :: _, Left) = SOME p
                    |   find(PTnextSibling p :: _, Right) = SOME p
                    |   find(PTfirstChild p :: _, Down) = SOME p
                    |   find(_ :: tl, dir) = find (tl, dir)
                in
                    if startLocn = startPosition andalso endLocn = endPosition
                    then (* We're there already. *) ()
                    else if startLocn >= startPosition andalso endLocn <= endPosition
                    then (* It's this node or a child. *)
                        let
                            val child = find(tree, Down)
                        in
                            (* See if the element we want is actually a child. *)
                            case child of
                                SOME child =>
                                let
                                    (* See which child it is. *)
                                    fun findChild(location as {startPosition, endPosition, ...}, child) =
                                        if startLocn >= startPosition andalso endLocn <= endPosition
                                        then SOME (location, child)
                                        else
                                        case find(child, Right) of
                                            NONE => NONE
                                        |   SOME next => findChild(next())
                                in
                                    case findChild(child()) of
                                        NONE => () (* In this *)
                                    |   SOME child =>
                                        (
                                            lastParsetree := SOME child;
                                            navigateTo(startLocn, endLocn)
                                        )
                                end
                            |   NONE => () (* No children. *)
                        end
                    else (* Must go out. *)
                    (
                        case find(tree, Up) of
                            SOME p =>
                            (
                                lastParsetree := SOME(p());
                                navigateTo(startLocn, endLocn)
                            )
                        |   NONE => () (* Not found *)
                    )
                end
 
            (* Main protocol loop. *)
            fun runProtocol () =
            let
                open PolyML (* Open first so we get TextIO.print not PolyML.print. *)
                open TextIO

                fun readToEscape (soFar: string) : string * char option =
                case input1 stdIn of
                    SOME #"\u001b" => (soFar, input1 stdIn)
                |   SOME ch => readToEscape(soFar ^ str ch)
                |   NONE => ("", NONE)

                (* Parse an integer.  Returns zero if it isn't a valid int. *)
                fun getOffset (): int * char option =
                let
                    val (str, term) = readToEscape ""
                    val n = case Int.fromString str of NONE => 0 | SOME i => i
                in
                    (n, term)
                end

                (* Read until we get ESC ch where ch is the appropriate terminator. *)
                fun skipToTerminator termCh =
                let
                    val (_, term) = readToEscape ""
                in
                    case term of
                        SOME ch => if ch = termCh then () else skipToTerminator termCh
                    |   NONE => ()
                end

                (* Reads a packet containing two locations and selects the tree containing them. *)
                fun gotoPosition termCh =
                    case getOffset () of
                        (startOffset, SOME #",") =>
                        (
                            case getOffset () of
                                (endOffset, SOME ch) =>
                                    if ch = termCh
                                    then navigateTo(startOffset, endOffset)
                                    else skipToTerminator termCh
                            |   _ => ()
                            
                        )
                    |   (_, SOME ch) => if ch = termCh then () else skipToTerminator termCh
                    |   (_, NONE) => ()

                fun endPacket endCh = (print(String.implode[#"\u001b", endCh]); TextIO.flushOut TextIO.stdOut)

            in
                case input1 stdIn of
                    NONE => () (* EOF - exit *)
                |   SOME #"\u001b" => (* Escape- start of packet. *)
                (
                    case input1 stdIn of
                        NONE => () (* EOF - exit *)
                    |   SOME ch =>
                    (
                        case ch of
                            #"R" =>
                            let (* Compile request. *)
                                (* Begin a new compilation. *)
                                (* Parameters are: load file, source file, start position and then
                                   the source text itself.*)
                                val (loadFile, term1) = readToEscape ""
                                val (sourceFile, term2) =
                                    case term1 of
                                        SOME #"," => readToEscape ""
                                    |   _ => ("", term1)
                                val (startOffset, term3) =
                                    case term2 of
                                        SOME #"," => getOffset()
                                    |   _ => (0, term2)

                                val errorList = ref []
                                val resultTrees = ref (SOME [])

                    			fun compilerResultFun (parsetree, codeOpt) =
                                let
                                    (* Put in the results without printing. *)
                                    fun resultFun
                                        { fixes: (string * fixityVal) list, values: (string * valueVal) list,
                                          structures: (string * structureVal) list, signatures: (string * signatureVal) list,
                                          functors: (string * functorVal) list, types: (string * typeVal) list} =
                                    let
                                    in
                                        List.app (#enterFix globalNameSpace) fixes;
                                        List.app (#enterType globalNameSpace) types;
                                        List.app (#enterSig globalNameSpace) signatures;
                                        List.app (#enterStruct globalNameSpace) structures;
                                        List.app (#enterFunct globalNameSpace) functors;
                                        List.app (#enterVal globalNameSpace) values
                                    end
                                in
                                    (* Add the parsetree to the list.  If we have a parse error and can't
                                       return a tree we remove any trees for previous topdecs and don't
                                       replace any existing tree when the compilation ends. *)
                                    case (parsetree, ! resultTrees) of
                                        (SOME p, SOME l) => resultTrees := SOME(l @ [p])
                                    |   _ => ();
                    			    case codeOpt of
                    					SOME code => (fn () => resultFun(code()))
                    				 |	NONE => raise Fail "Static Errors"
                                end
            
                                val byteCount    = ref startOffset
                                val endMarkerFound = ref false
 
                                (* Read characters and return them to the compiler unless they are an escape combination. *)
                                fun readin() =
                                    case TextIO.input1 TextIO.stdIn of
                                        NONE => (endMarkerFound := true; NONE)
                                    |   SOME #"\u001b" =>
                                        ( (* Escape: Look at next character. *)
                                            case TextIO.input1 TextIO.stdIn of
                                                NONE => (endMarkerFound := true; NONE)
                                            |   SOME #"\u001b" => (* Stuffed escape.  Could appear in a comment? *)
                                                (byteCount := !byteCount+1; SOME #"\u001b")
                                            |   SOME #"r" => (* Normal end character. *)
                                                (endMarkerFound := true; NONE)
                                            |   SOME ch => (* Shouldn't happen: treat as end. *)
                                                (endMarkerFound := true; NONE)
                                        )
                                    |   SOME ch => (byteCount := !byteCount+1; SOME ch)
            
                                fun compilerLoop () =
                                (* Compile each top dec until either we get the end marker or an exception. *)
                                if ! endMarkerFound then ()
                                else
                                let
                                    val code =
                                        SOME(polyCompiler(readin,
                                            [CPOutStream printOut, CPLineOffset (fn () => !byteCount),
                                             CPErrorMessageProc (fn msg => errorList := !errorList @ [msg]),
                                             CPCompilerResultFun compilerResultFun, CPFileName sourceFile]))
                                            handle _ => NONE
                                    val result =
                                    case code of
                                        SOME code =>
                                        (
                                            (code (); true)
                                            (* Report exceptions in running code. TODO: Report this as an error. *)
                                            handle exn =>
                                                (
                                                    printOut ("Exception- " ^ PolyML.makestringInNameSpace(exn, globalNameSpace)
                                                        ^ " raised\n");
                                                    false
                                                )
                                        )
                                    |   NONE => false
                                in
                                    if result then compilerLoop ()
                                    else ()
                                end
                            in
                                case term3 of
                                    SOME #"," =>
                                    (
                                        (* First reload the state.  If it raises an exception stop here. *)
                                        if loadFile <> "" andalso
                                            ((PolyML.SaveState.loadState loadFile; false) handle _ => true)
                                        then (* Load error. *)
                                            ( print "\u001bRL\u001b,"; print(Int.toString(! byteCount)); endPacket #"r" )
                                        else
                                        (
                                            (* Now compile the source until either we get the end
                                               marker or an exception. *)
                                            compilerLoop ();
                                            (* If we had an exception skip until we get the end marker. *)
                                            while not (! endMarkerFound) do readin();

                                            (* Print the result packet containing error messages. *)
                                            print "\u001bR"; (* Escape R - start of packet. *)
                                            case (! errorList, ! resultTrees) of
                                                ([], _) => print "S" (* No errors *)
                                            |   (_, NONE) => print "P" (* Errors and no trees *)
                                            |   _  => print "F"; (* Errors but have trees. *)
                                            print "\u001b,";
                                            print(Int.toString(! byteCount));

                                            let
                                                fun printErr {message: PolyML.pretty, hard: bool,
                                                              location = {startPosition, endPosition, ...}, ...} =
                                                (
                                                    print(String.concat
                                                        [
                                                            "\u001bE",
                                                            if hard then "E" else "W", "\u001b,",
                                                            Int.toString startPosition, "\u001b,",
                                                            Int.toString endPosition, "\u001b,"
                                                        ]);
                                                    prettyPrintWithMarkup(print, !lineLength)
                                                        ((* Always add prefixes. *)addStructurePrefix true message);
                                                    endPacket #"e" (* Escape e - end *)
                                                )
                                            in
                                                List.app printErr (! errorList)
                                            end;
                                            case ! resultTrees of SOME trees => parseTrees := trees | NONE => ();
                                            (* Set the current parse tree pointer to the first tree. *)
                                            case ! parseTrees of
                                                [] => lastParsetree := NONE
                                            |   hd :: _ => lastParsetree := SOME hd;
                                            endPacket #"r" (* Escape-r - end of compilation. *)
                                        )
                                    )
                                |   SOME #"r" => (* Reached terminator too early. *)
                                        (print "\u001bR"; endPacket #"r")
                                |   _ => (* Protocol error or end-of-stream. *)
                                        (skipToTerminator #"r"; print "\u001bR"; endPacket #"r")
                            end

                            (* Navigation functions. *)
                        |   #"U" => (gotoPosition #"u"; navigate Up; printLocation #"U"; endPacket #"u")
                        |   #"C" => (gotoPosition #"c"; navigate Down; printLocation #"C"; endPacket #"c")
                        |   #"N" => (gotoPosition #"n"; navigate Right; printLocation #"N"; endPacket #"n")
                        |   #"P" => (gotoPosition #"p"; navigate Left; printLocation #"P"; endPacket #"p")
                            (* Print the type of the selected node. *)
                        |   #"T" =>
                            (
                                gotoPosition #"t"; printLocation #"T";
                                case lastParsetree of
                                    ref NONE => ()
                                |   ref (SOME(_, tree)) =>
                                    (
                                        (* Print the type if it's there.  Don't include any mark-up. *)
                                        case List.find (fn (PTtype p) => true | _ => false) tree of
                                            SOME(PTtype t) =>
                                            (
                                                print "\u001b,";
                                                PolyML.prettyPrint(printOut, !lineLength)
                                                    (PolyML.NameSpace.displayTypeExpression(t, 100))
                                            )
                                        |   _ => ()
                                    );
                                endPacket #"t"
                            )
                            (* Print the declaration location of the selected node. *)
                        |   #"I" =>
                            (
                                gotoPosition #"i"; printLocation #"I";
                                case lastParsetree of
                                    ref NONE => ()
                                |   ref (SOME(_, tree)) =>
                                    (
                                        (* Print the declaration location if it's there. *)
                                        case List.find (fn (PTdeclaredAt p) => true | _ => false) tree of
                                            SOME(PTdeclaredAt
                                                {file, startLine, startPosition, endPosition, ...}) =>
                                            (
                                                print "\u001b,";
                                                print file; (* TODO double any escapes. *) print "\u001b,";
                                                print (Int.toString startLine); print "\u001b,";
                                                print (Int.toString startPosition); print "\u001b,";
                                                print (Int.toString endPosition)
                                            )
                                        |   _ => ()
                                    );
                                endPacket #"i"
                            )

                        |   #"O" => (* Print list of valid commands. *)
                            (
                                gotoPosition #"o"; printLocation #"O";
                                case lastParsetree of
                                    ref NONE => ()
                                |   ref (SOME(_, tree)) =>
                                    let
                                        fun printCode(PTparent _) = print "U"
                                        |   printCode(PTpreviousSibling _) = print "P"
                                        |   printCode(PTnextSibling _) = print "N"
                                        |   printCode(PTfirstChild _) = print "C"
                                        |   printCode(PTtype _) = print "T"
                                        |   printCode(PTdeclaredAt _) = print "I"
                                        |   printCode(PTprint _) = ()
                                    in
                                        List.app printCode tree
                                    end;
                                endPacket #"o"
                            )

                        |   ch => (* Something else.  Reply with empty response. *)
                            let
                                val term = Char.toLower ch
                            in
                                skipToTerminator term; printLocation ch; endPacket term
                            end;

                        runProtocol() (* Continue. *)
                    )
                )
                |   SOME _ => runProtocol() (* Discard other characters. *)
            end
        in
            runProtocol ()
        end (* background compilation. *)

        else (* Enter normal Poly/ML top-level. *)
        let
            open Signal;
            val () =
                if List.exists(fn s => s = "-q") (CommandLine.arguments())
                then ()
                else print (String.concat ["Poly/ML ", Bootstrap.compilerVersion, "\n"]);
            (* Set up a handler for SIGINT if that is currently set to SIG_DFL.
               If a handler has been set up by an initialisation function don't replace it. *)
            val () =
                case signal(2, SIG_IGN) of
                   SIG_IGN => ()
                |  SIG_DFL => (signal(2, SIG_HANDLE(fn _ => Thread.Thread.broadcastInterrupt())); ())
                |  oldHandle => (signal(2, oldHandle); ())
            (* See whether the caller can handle mark-up. *)
            val addMarkup = List.exists(fn s => s = "--with-markup") (CommandLine.arguments())
        in
            shell addMarkup;
            OS.Process.exit OS.Process.success (* Run any "atExit" functions and then quit. *)
        end
    end;

in
    structure PolyML =
    struct
        open PolyML
        (* We must not have a signature on the result otherwise print and makestring
           will be given polymorphic types and will only produce "?" *)

        val globalNameSpace = globalNameSpace

        val use = use and make = make
        val suffixes = suffixes
        val compiler = polyCompiler
        
        val saveDirectory = saveDirectory
        and dependencies = dependencies

        (* Main root function: run the main loop. *)
        val rootFunction: unit->unit = rootShell

        structure Compiler =
        struct
            datatype compilerParameters = datatype compilerParameters

            val compilerVersion = Bootstrap.compilerVersion

            val forgetSignature: string -> unit = forgetSig
            and forgetStructure: string -> unit = forgetStruct
            and forgetFunctor: string -> unit = forgetFunct
            and forgetValue: string -> unit = forgetVal
            and forgetType: string -> unit = forgetType
            and forgetFixity: string -> unit = forgetFix

            fun signatureNames (): string list = #1(ListPair.unzip (#allSig globalNameSpace ()))
            and structureNames (): string list = #1(ListPair.unzip (#allStruct globalNameSpace ()))
            and functorNames (): string list = #1(ListPair.unzip (#allFunct globalNameSpace ()))
            and valueNames (): string list = #1(ListPair.unzip (#allVal globalNameSpace ()))
            and typeNames (): string list = #1(ListPair.unzip (#allType globalNameSpace ()))
            and fixityNames (): string list = #1(ListPair.unzip (#allFix globalNameSpace ()))

            val prompt1 = prompt1 and prompt2 = prompt2 and profiling = profiling
            and timing = timing and printDepth = printDepth
            and errorDepth = errorDepth and lineLength = lineLength
            
            val assemblyCode = assemblyCode and codetree = codetree
            and codetreeAfterOpt = codetreeAfterOpt and pstackTrace = pstackTrace
            and parsetree = parsetree
            
            val debug = debug
            val inlineFunctors = inlineFunctors
            val maxInlineSize = maxInlineSize
            val printInAlphabeticalOrder = printInAlphabeticalOrder
            val printTypesWithStructureName = printTypesWithStructureName
            val traceCompiler = traceCompiler
        end
        
        and Debug =
        struct
            (* singleStep causes the debugger to be entered on the next call.
               stepOver enters the debugger on the next call when the stack is no larger
               than it is at present.
               stepOut enters the debugger on the next call when the stack is smaller
               than it is at present. *)
            fun step () = (stepDebug := true; stepDepth := ~1; exitLoop := true)
            and stepOver() = (stepDebug := true; stepDepth := List.length(!(getStack())); exitLoop := true)
            and stepOut() = (stepDebug := true; stepDepth := List.length(!(getStack())) - 1; exitLoop := true)
            and continue () = (stepDebug := false; stepDepth := ~1; exitLoop := true)
            and continueWithEx exn =
                (stepDebug := false; stepDepth := ~1; exitLoop := true; debugExPacket := SOME exn)
            and trace b = tracing := b

            fun breakAt (file, line) =
                if checkLineBreak(file, line) then () (* Already there. *)
                else lineBreakPoints := (file, line) :: ! lineBreakPoints
        
            fun clearAt (file, line) =
            let
                fun findBreak [] = (TextIO.print "No such breakpoint.\n"; [])
                 |  findBreak ((f, l) :: rest) =
                      if l = line andalso f = file
                      then rest else (f, l) :: findBreak rest
            in
                lineBreakPoints := findBreak (! lineBreakPoints)
            end
         
            fun breakIn name =
                if checkFnBreak true name then () (* Already there. *)
                else fnBreakPoints := name :: ! fnBreakPoints
        
            fun clearIn name =
            let
                fun findBreak [] = (TextIO.print "No such breakpoint.\n"; [])
                 |  findBreak (n :: rest) =
                      if name = n then rest else n :: findBreak rest
            in
                fnBreakPoints := findBreak (! fnBreakPoints)
            end

            fun breakEx exn =
                if checkExnBreak exn then  () (* Already there. *)
                else exBreakPoints := getExnId exn :: ! exBreakPoints

            fun clearEx exn =
            let
                val exnId = getExnId exn
                fun findBreak [] = (TextIO.print "No such breakpoint.\n"; [])
                 |  findBreak (n :: rest) =
                      if exnId = n then rest else n :: findBreak rest
            in
                exBreakPoints := findBreak (! exBreakPoints)
            end

            (* Stack traversal. *)
            fun up () =
            let
                val stack = getStack()
            in
                if !debugLevel < List.length (!stack) -1
                then
                let
                    val _ = debugLevel := !debugLevel + 1;
                    val {funName, lineNo, fileName, ...} = List.nth(!stack, !debugLevel)
                in
                    printSourceLine(fileName, lineNo, funName)
                end
                else TextIO.print "Top of stack.\n"
            end
        
            and down () =
            let
                val stack = getStack()
            in
                if !debugLevel = 0
                then TextIO.print "Bottom of stack.\n"
                else
                let
                    val () = debugLevel := !debugLevel - 1;
                    val {funName, lineNo, fileName, ...} = List.nth(!stack, !debugLevel)
                in
                    printSourceLine(fileName, lineNo, funName)
                end
            end

            (* Just print the functions without any other context. *)
            fun stack () : unit =
            let
                fun printTrace {funName, lineNo, fileName, ...} =
                (
                    if fileName = "" then () else TextIO.print(concat[fileName, " "]);
                    if lineNo = 0 then () else TextIO.print(concat[" line:", Int.toString lineNo, " "]);
                    TextIO.print(concat["function:", funName, "\n"])
                )
            in
                List.app printTrace (! (getStack()))
            end

            local
                fun printVal v =
                    prettyPrintWithMarkup(TextIO.print, !lineLength)
                        (NameSpace.displayVal(v, !printDepth, globalNameSpace))
                fun printStack stack =
                    List.app (fn (_,v) => printVal v) (#allVal (#space stack) ())
            in
                (* Print all variables at the current level. *)
                fun variables() = printStack (List.nth(!(getStack()), !debugLevel))
                (* Print all the levels. *)
                and dump() =
                let
                    fun printLevel (stack as {funName, ...}) =
                    (
                        TextIO.print(concat["Function ", funName, ":"]);
                        printStack stack;
                        TextIO.print "\n"
                    )
                in
                    List.app printLevel (!(getStack()))
                end
            end
        end
        
        (* Original print_depth etc functions. *)
        fun profiling   i = Compiler.profiling := i
        and timing      b = Compiler.timing := b
        and print_depth i = Compiler.printDepth := i
        and error_depth i = Compiler.errorDepth := i
        and line_length i = Compiler.lineLength := i
    end
end (* PolyML. *);

val use = PolyML.use;

(* Copy everything out of the original name space. *)
(* Do this AFTER we've finished compiling PolyML and after adding "use". *)
val () = List.app (#enterVal PolyML.globalNameSpace) (#allVal Bootstrap.globalSpace ())
and () = List.app (#enterFix PolyML.globalNameSpace) (#allFix Bootstrap.globalSpace ())
and () = List.app (#enterSig PolyML.globalNameSpace) (#allSig Bootstrap.globalSpace ())
and () = List.app (#enterType PolyML.globalNameSpace) (#allType Bootstrap.globalSpace ())
and () = List.app (#enterFunct PolyML.globalNameSpace) (#allFunct Bootstrap.globalSpace ())
and () = List.app (#enterStruct PolyML.globalNameSpace) (#allStruct Bootstrap.globalSpace ())

(* We don't want Bootstrap copied over. *)
val () = PolyML.Compiler.forgetStructure "Bootstrap";

