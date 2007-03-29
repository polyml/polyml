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
 
structure MLCompiler =
struct
  local
    open RuntimeCalls;
	  local
	     open StructVals
		 open Make
	     (* Create the global (top-level) environment *)
	     val globalUnivTable = structSignat (makeEmptyGlobal "global");
	  in
	     (* create a functional view of it *)
	     val globalEnv = makeEnv globalUnivTable;
	   
	     (* Wrap the environment.  I'm not sure this is still needed. *)
	     val globalTable : dbEnv = makeDbEnv (sigTab globalUnivTable);
	  end;
	  
	val mainShell = Make.shellProc (globalTable, Debug.prompt1, Debug.prompt2);
	
	val mainSessionShell = #shell mainShell

    val debugEnv = Make.makeSubshellDbEnv globalTable;
	 
    val {debugger, ...} =
        Make.shellProc (debugEnv, ref "debug> ", ref "debug# ");
    (* Link up the debugger. *)
    val _ = Debugger.setDebugger debugger;

	fun printVersion() = ()
		
    (* create the main shell for exporting *)
    (* the primary process group *)
    fun mainGo () : unit = 
    (
	    if List.exists(fn s => s = "-q") (CommandLine.arguments())
		then ()
		else (!Debug.printString) (String.concat ["Poly/ML ", CompilerVersion.compilerVersion, "\n"]);
      mainSessionShell ();
	  OS.Process.exit OS.Process.success (* Run any "atExit" functions and then quit. *)
    );

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
    		(!Debug.printString)
			     (String.concat ["Poly/ML ", PolyML.Compiler.compilerVersion, 
			                     "    RTS version: ", rtsRelease(), "\n"])

		else if List.exists(fn s => s = "--help") argList
		then (* --help option: Print argument information and exit. *)
		   (
     		(!Debug.printString) (String.concat ["Poly/ML ", CompilerVersion.compilerVersion, "\n"]);
			(!Debug.printString) "Compiler arguments:\n";
			(!Debug.printString) "\n";
			(!Debug.printString) "-v        Print the version of Poly/ML and exit\n";
			(!Debug.printString) "--help    Print this message and exit\n";
			(!Debug.printString) "-q        Suppress the start-up message\n";
			(!Debug.printString) "\nRun time system arguments:\n";
			(!Debug.printString) (rtsHelp())
		   )
		else
             mainGo ()
	end;

    val U:unit =
	    Initialise.initGlobalEnv (mainShell, globalEnv, globalTable, rootShell);
  in

    val make = Make.makeIntoEnv globalTable; (* I don't think this is used. *)
    val use  = Make.useIntoEnv globalTable; (* Used to compile in the prelude and basis. *)
    
    val shell = rootShell
  end;
end;
