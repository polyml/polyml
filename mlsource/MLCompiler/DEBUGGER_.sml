(*
    Title:      Source level debugger for Poly/ML
    Author:     David Matthews
    Copyright  (c)   David Matthews 2000

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

functor DEBUGGER_ (

(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS :
sig
  type types
  type typeConstrs
  type fixStatus
  type structVals
  type signatures
  type functors
  type values
  type codetree
  type valAccess
  val Global: codetree -> valAccess
  val makeValueConstr: string * types * bool * valAccess -> values;
end;

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
  val printString : (string->unit) ref
  val printDepth : int ref
end;

(*****************************************************************************)
(*                  VALUEOPS                                                 *)
(*****************************************************************************)
structure VALUEOPS :
sig
  type codetree
  type types
  type values
  type fixStatus
  type structVals
  type prettyPrinter
  type machineWord

  val mkGvar:    string * types * codetree -> values
  val mkGex:     string * types * codetree -> values
  
  val printStruct: machineWord * types * int * prettyPrinter -> unit
end

structure CODETREE :
sig
  type machineWord
  type codetree
  val mkConst:          machineWord -> codetree;
end

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
  val ppEliding    : prettyPrinter -> unit -> bool
  val ppEndStream  : prettyPrinter -> unit -> unit
  val ppLineBreak  : prettyPrinter -> unit -> unit
  val prettyPrint : int * (string -> unit) -> prettyPrinter; 
end;

sharing type
  CODETREE.machineWord
= VALUEOPS.machineWord

sharing type
  CODETREE.codetree
= VALUEOPS.codetree
= STRUCTVALS.codetree

sharing type
  STRUCTVALS.values 
= VALUEOPS.values

sharing type
  STRUCTVALS.types 
= VALUEOPS.types

sharing type
  PRETTYPRINTER.prettyPrinter 
= VALUEOPS.prettyPrinter
)
:
sig
    type types
	type values
	type machineWord
    type fixStatus
    type structVals
    type typeConstrs
    type signatures
    type functors

	datatype environEntry =
		EnvValue of string * types
	|	EnvException of string * types
	|	EnvVConstr of string * types * bool
	|	EnvStaticLevel

	val debugFunction:
		string * string * int -> environEntry list -> machineWord list -> unit
	val enterFunction: string -> unit -> unit
	val leaveFunction: types -> machineWord -> machineWord
	val exceptionFunction: exn -> 'a
	val setDebugger: ({lookupVal: string -> values option, lookupType: string -> typeConstrs option,
                    lookupFix: string -> fixStatus option, lookupStruct: string -> structVals option,
                    lookupSig: string -> signatures option, lookupFunct:  string -> functors option }
                    *(unit->bool)->unit) -> unit
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
end
=
struct
    open STRUCTVALS DEBUG VALUEOPS CODETREE PRETTYPRINTER

	(* The debugger is actually the compiler itself but we need the
	   address of the debugFunction within the compiler.  To close
	   the loop we have this reference which is set when the compiler
	   has been compiled. *)
	val debugRef: ({lookupVal: string -> values option, lookupType: string -> typeConstrs option,
                    lookupFix: string -> fixStatus option, lookupStruct: string -> structVals option,
                    lookupSig: string -> signatures option, lookupFunct:  string -> functors option }
                    *(unit->bool)->unit) ref = ref (fn _ => ())
	fun setDebugger debug = (debugRef := debug)

	val exitLoop = ref false; (* Set to true to exit the debug loop *)

	(* Call tracing. *)
    val tracing = ref false;
	val enteredFn = ref false;
	fun traceFunctions b = tracing := b

	(* The static environment contains these kinds of entries. *)
	datatype environEntry =
		EnvValue of string * types
	|	EnvException of string * types
	|	EnvVConstr of string * types * bool
	|	EnvStaticLevel

	(* Whenever we enter a function we push information onto this stack. *)
	type stackEntry =
	{
		lineNo: int,
		funName: string,
		fileName: string,
		ctEnv: environEntry list,
		rtEnv: machineWord list
	}

	val stack: stackEntry list ref = ref []
	val ctLast = ref [] and rtLast = ref [] and lastFun = ref ""
	and lastLine = ref 0 and lastFile = ref ""
	val debugLevel = ref 0

	(* Single stepping. *)
	val step = ref false;
	val stepDepth = ref ~1; (* Only break at a stack size less than this. *)

	(* singleStep causes the debugger to be entered on the next call.
	   stepOver enters the debugger on the next call when the stack is no larger
	   than it is at present.
	   stepOut enters the debugger on the next call when the stack is smaller
	   than it is at present. *)
	fun singleStep () = (step := true; stepDepth := ~1; exitLoop := true)
	and stepOver() = (step := true; stepDepth := List.length(!stack); exitLoop := true)
	and stepOut() = (step := true; stepDepth := List.length(!stack) - 1; exitLoop := true)
	and continue () = (step := false; stepDepth := ~1; exitLoop := true);

	(* Break points.  We have two breakpoint lists: a list of file-line
	   pairs and a list of function names. *)
	val lineBreakPoints = ref []
	and fnBreakPoints = ref []
	
	fun checkLineBreak (file, line) =
	let
		fun findBreak [] = false
		 |  findBreak ((f, l) :: rest) =
		 	  (l = line andalso f = file) orelse findBreak rest
	in
		findBreak (! lineBreakPoints)
	end

	fun addLineBreak (file, line) =
		if checkLineBreak(file, line) then () (* Already there. *)
		else lineBreakPoints := (file, line) :: ! lineBreakPoints

	fun clearLineBreak (file, line) =
	let
		fun findBreak [] = ((!printString) "No such breakpoint.\n"; [])
		 |  findBreak ((f, l) :: rest) =
		 	  if l = line andalso f = file
			  then rest else (f, l) :: findBreak rest
	in
		lineBreakPoints := findBreak (! lineBreakPoints)
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

	fun addFnBreak name =
		if checkFnBreak true name then () (* Already there. *)
		else fnBreakPoints := name :: ! fnBreakPoints

	fun clearFnBreak name =
	let
		fun findBreak [] = ((!printString) "No such breakpoint.\n"; [])
		 |  findBreak (n :: rest) =
		 	  if name = n then rest else n :: findBreak rest
	in
		fnBreakPoints := findBreak (! fnBreakPoints)
	end

	(* Look up a name in the environment.  If it isn't there
	   this function raises an exception which will cause a
	   search of the global environment. *)
	fun valueLookup (valEnv, valueList) s =
	let
		fun searchList (EnvValue(name, ty) :: ntl, valu :: vl) =
		  		if name = s
				then mkGvar(name, ty, mkConst valu)
				else searchList(ntl, vl)

		  |  searchList (EnvException(name, ty) :: ntl, valu :: vl) =
		  		if name = s
				then mkGex(name, ty, mkConst valu)
				else searchList(ntl, vl)

		  |  searchList (EnvVConstr(name, ty, nullary) :: ntl, valu :: vl) =
		  		if name = s
				then makeValueConstr(name, ty, nullary, Global(mkConst valu))
				else searchList(ntl, vl)

		  |  searchList (EnvStaticLevel :: ntl, vl) =
		  		(* Static level markers have no effect here. *)
		  		searchList(ntl, vl)

		  | searchList _ =
		  	 (* The name we are looking for isn't in
			    the environment.
				The lists should be the same length. *)
			 raise Subscript
	in
		SOME(if ! debugLevel = 0
		then searchList(valEnv, valueList)
		else (* Find the entry in the stack. *)
		let
			val {ctEnv, rtEnv, ...} = List.nth(!stack, !debugLevel -1)
		in
			searchList(ctEnv, rtEnv)
		end) handle Subscript => NONE
	end

	(* Try to print the appropriate line from the file. *)
	fun printLine(fileName: string, line: int, funName: string) =
	(
	(* First just print where we are. *)
	(! printString)(
		concat[fileName, " line:", Int.toString line, " function:", funName, "\n"]);
	(* Try to print it.  This may fail if the file name was not a full path
	   name and we're not in the correct directory. *)
	let
		val fd = TextIO.openIn fileName
		fun pLine n =
			case TextIO.inputLine fd of
				NONE => ()
			|	SOME s => if n = 1 then (! printString) s else pLine(n-1)
	in
		pLine line;
		TextIO.closeIn fd
	end handle IO.Io _ => ()) (* If it failed simply ignore the error. *)

	(* Stack traversal. *)
	fun upStack () =
		if !debugLevel < List.length (!stack) -1
		then
		let
			val _ = debugLevel := !debugLevel + 1;
			val {funName, lineNo, fileName, ...} = List.nth(!stack, !debugLevel -1)
		in
			printLine(fileName, lineNo, funName)
		end
		else (!printString) "Top of stack.\n"

	and downStack () =
		if !debugLevel = 0
		then (!printString) "Bottom of stack.\n"
		else if !debugLevel = 1
		then
			(
			debugLevel := 0;
			printLine(!lastFile, !lastLine, !lastFun)
			)
		else
		let
			val _ = debugLevel := !debugLevel - 1;
			val {funName, lineNo, fileName, ...} = List.nth(!stack, !debugLevel -1)
		in
			printLine(fileName, lineNo, funName)
		end

	fun printValue pstream (t: types, v: machineWord) =
		printStruct(v, t, ! DEBUG.printDepth, pstream)

	local
		fun printAll pstream (EnvValue(name, ty) :: ctRest, value :: rtRest) doAll =
			(
				ppBeginBlock pstream (0, false);
				ppAddString pstream ("val " ^ name ^ " =");
				ppBreak pstream (1, 2);
				printValue pstream (ty, value);
				ppEndBlock pstream ();
				ppBreak pstream (1, 0);
				printAll pstream (ctRest, rtRest) doAll
			)
		  | printAll pstream (EnvException(name, ty) :: ctRest, value :: rtRest) doAll=
			(
				ppAddString pstream ("exception " ^ name);
				ppBreak pstream (1, 0);
				printAll pstream (ctRest, rtRest)  doAll
			)
		  | printAll pstream (EnvVConstr(name, _, _) :: ctRest, value :: rtRest) doAll =
			(
				(* We really want to include these with the type. *)
				ppAddString pstream ("constructor " ^ name);
				ppBreak pstream (1, 0);
				printAll pstream (ctRest, rtRest) doAll
			)
		  | printAll pstream (EnvStaticLevel :: ctRest, rtRest) doAll =
		  		if doAll then printAll pstream (ctRest, rtRest) doAll
				else () (* Just the locals. *)
		  | printAll _ _ _ = ()

		fun printFunction pstream (fName, ctEnv, rtEnv) =
		(
			ppBeginBlock pstream (0, false);
			ppAddString pstream (concat["Function ", fName, ":"]);
			ppBreak pstream (1, 2);
			printAll pstream (ctEnv, rtEnv) true;
			ppEndBlock pstream ()
		)
	in
		(* dumpStack - print all variables for every function. *)
		fun dumpStack () : unit =
		let
			val pstream = prettyPrint (77, ! printString)

			fun printStackEntries nil = ()
			  | printStackEntries [_] = () (* Ignore the bottom entry. *)
			  | printStackEntries (({ctEnv, rtEnv, funName, ...}: stackEntry):: printRest) =
			(
				printFunction pstream (funName, ctEnv, rtEnv);
				ppLineBreak pstream ();
				printStackEntries printRest
			)
		in
			ppBeginBlock pstream (0, false);
			printFunction pstream (! lastFun, !ctLast, !rtLast);
			ppLineBreak pstream ();
			printStackEntries (!stack);
			ppEndBlock pstream ()
		end

		and stackTrace () : unit =
		let
			fun printTrace (funName, lineNo, fileName) =
				(! printString)(
					concat[fileName, " line:", Int.toString lineNo, " function:", funName, "\n"])

			fun printStackEntries nil = ()
			  | printStackEntries [_] = () (* Ignore the bottom entry. *)
			  | printStackEntries (({funName, lineNo, fileName, ...}: stackEntry):: printRest) =
			(
				printTrace (funName, lineNo, fileName);
				printStackEntries printRest
			)
		in
			printTrace (! lastFun, !lastLine, !lastFile);
			printStackEntries (!stack)
		end

		and printVars () = (* Print all the variables for this function. *)
		let
			val pstream = prettyPrint (77, ! printString)
		in
			ppBeginBlock pstream (0, false);
			if ! debugLevel = 0
			then printAll pstream (!ctLast, !rtLast) true
			else (* Find the entry in the stack. *)
			let
				val {ctEnv, rtEnv, ...} = List.nth(!stack, !debugLevel -1)
			in
				printAll pstream (ctEnv, rtEnv) true
			end;
			ppEndBlock pstream ()
		end

		and printArgs pstream env = (* Print the function arguments only *)
			printAll pstream env false
	end

	val alreadyInDebugger = ref false; (* Prevents recursion. *)

	fun withoutRecursion f =
	(* If we have debugging turned on in a module that installs
	   a pretty-printer we could get an infinite recursion. If we
	   get a recursive call to the debugger we simply ignore it. *)
		if ! alreadyInDebugger
		then ()
		else (
			alreadyInDebugger := true;
			f ();
			alreadyInDebugger := false
			) handle exn => (alreadyInDebugger := false; raise exn)


	(* A pointer to this function is inserted in the code for each line. *)
	(* Although the nameTypeList and valueList are the same
	   length we build them separately.  This allows the
	   nameTypeList to be built at compile time and reduces
	   the run-time costs. *)
	fun debugFunction (fileName, _, line) staticEnv valueList =
	let
		fun debug  () =
		(
			ctLast := staticEnv; (* Remember these *)
			rtLast := valueList;
			lastLine := line;
			lastFile := fileName;
			(* We need to enter the debugger if we are single stepping or
			   we have a break at this line or
			   we have a break in this function and we've just entered it. *)
			if (!step andalso (!stepDepth < 0 orelse List.length(!stack) <= !stepDepth)) orelse
			   checkLineBreak (!lastFun, line) orelse
			   (!enteredFn andalso checkFnBreak false (!lastFun))
			then (* Break here. *)
				(
					exitLoop := false;
					debugLevel := 0;
					enteredFn := false;
					printLine(fileName, line, !lastFun);
                    (* We have a full environment here for future expansion but at
                       the moment only the value environment is used. *)
					(!debugRef) ({ lookupVal = valueLookup(staticEnv, valueList),
                                  lookupType = fn _ => NONE, lookupFix = fn _ => NONE,
                                  lookupStruct = fn _ => NONE, lookupSig = fn _ => NONE,
                                  lookupFunct = fn _ => NONE}, fn () => !exitLoop)
				)

			else if !enteredFn
			then (* First debug call of a function. *)
			(
				if !tracing
				then
					let
						(* This is the first debug call in the function.
						   We should now have enough information to print
						   the arguments. *)
						val pstream = prettyPrint (77, ! printString)
						val len = List.length(!stack)
					in
						ppBeginBlock pstream (len, true);
						ppBreak pstream (len, 0);
						ppAddString pstream (!lastFun ^ " entered");
						ppBreak pstream (1, 0);
						printArgs pstream (staticEnv, valueList);
						ppEndBlock pstream ()
					end
				else ();
				enteredFn := false
			)
			else ()
		)
	in
		withoutRecursion debug
	end

	(* The code for these functions is compiled in at the beginning and
	   end of every function. *)
	fun enterFunction (name: string) =
	let
		(* The function name supplied is made up to be suitable for output
		   when profiling.  We need to clean it up a bit for use here. The
		   general form is F().S.v-f(2)g where F is a functor, S a structure,
		   v a val declaration, f a curried function and g the function itself.
		   For the moment just strip out the argument numbers. *)
		fun checkChar (#")", (_, l)) = (true,  #")" :: l) (* Start of parens *)
		  | checkChar (#"(", (_, l)) = (false, #"(" :: l) (* End of parens *)
		  | checkChar (_, (true, l)) = (true, l) (* Remove the character *)
		  | checkChar (c, (false, l)) = (false, c :: l)
		val (_, chars) = List.foldr checkChar (false, []) (explode name)
		val processedName = String.implode chars
	in
		fn () =>
		withoutRecursion(
			fn () =>
				(
					(* Push the last environment onto the stack. *)
					stack :=
						{ctEnv = !ctLast,
						 rtEnv = !rtLast,
						 funName = !lastFun,
						 lineNo = !lastLine,
						 fileName = !lastFile} :: ! stack;
					lastFun := processedName;
					enteredFn := true
				)
			)
	end

	and leaveFunction resType fnResult =
		(
		withoutRecursion(
			fn () =>
			(
				case !stack of
				  [] => () (* Shouldn't happen. *)
				| {ctEnv, rtEnv, funName, lineNo, fileName} :: s =>
					(
					if ! tracing
					then
					(
					let
						val pstream = prettyPrint (77, ! printString)
					in
						alreadyInDebugger := true;
						ppBeginBlock pstream (List.length(!stack), false);
						ppBreak pstream (List.length(!stack), 0);
						ppAddString pstream (! lastFun ^ " returned");
						ppBreak pstream (1, 0);
						printValue pstream (resType, fnResult);
						ppEndBlock pstream ();
						alreadyInDebugger := false
					end) handle exn => (alreadyInDebugger := false; raise exn)
					else ();
					stack := s;
					ctLast := ctEnv;
					rtLast := rtEnv;
					lastFun := funName;
					lastLine := lineNo;
					lastFile := fileName
					)
			)
		) : unit;
		fnResult
		)

	fun exceptionFunction exn =
		(
		withoutRecursion(
			fn () =>
			(
				case !stack of
				  [] => () (* Shouldn't happen. *)
				| {funName, ...} :: s =>
					(
					if ! tracing
					then
					let
						val pstream = prettyPrint (77, ! printString)
					in
						ppBeginBlock pstream (List.length(!stack), true);
						ppBreak pstream (List.length(!stack), 0);
						ppAddString pstream (concat[funName, ": exception ", exnName exn]);
						ppEndBlock pstream ()
					end
					else ();
					stack := s
					)
				)
			) : unit;
		raise exn
		)

end;
