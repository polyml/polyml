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
    
    fun systemCommit () : bool = RunCall.run_call0 POLY_SYS_commit ()

    datatype envShellOption = 
      None
    | Some of Initialise.dbEnv * (unit -> unit);

    val mainEnvShell = ref None;
    
    (* create the main and secondary shells for the database *)
    fun rootShell () =
      case !mainEnvShell of
        None => raise Misc.InternalError "Shell environment not set"
      | Some (mainEnv, sessionShell) =>
	let
	  val secondaryEnv = Make.makeSubshellDbEnv mainEnv;
	  val {shell = secondarySessionShell, ...} =
	     Make.shellProc (secondaryEnv, ref "2> ", ref "2# ");
	  val {debugger, ...} =
	     Make.shellProc (secondaryEnv, ref "debug> ", ref "debug# ");
	  (* Link up the debugger. *)
	  val _ = Debugger.setDebugger debugger;
	
	  (* print message, run a session-shell, then quit *)
	  fun mainLoop sessionShell true  =
	  		OS.Process.exit OS.Process.success (* Run any "atExit" functions and then quit. *)
	    | mainLoop sessionShell false = 
	       (
		   	 (* Runs the start-up function every time. *)
			 (!Initialise.startupFn) ();
			 sessionShell ();
			 (* When the shell finishes i.e. ^D has been typed we call commit.  This returns
			    true when the commit has been completed but leaves the database in the state
				in which the commit is about to return.  The next time we run the database
				the commit does return but the value it returns is false.  In that case we
				start a new session.  If the commit fails (currently only because the database
				has been opened for read-only) we ignore the error. *)
			 mainLoop sessionShell (systemCommit () handle _ => true)
	       );
    
	  (* We have to start the initial functions in BOTH
	     the primary and secondary process groups as
	     otherwise the security process wouldn't be running
	     while we had switched to the secondary shell. *)
	  (* the secondary process group *)
	  fun secondaryGo () =
	    (
	      (!Initialise.initialFn) false;
	      mainLoop secondarySessionShell false
	    );
    
	  (* the primary process group *)
	  fun mainGo () : unit = 
	    (
	      RunCall.run_call1 POLY_SYS_install_subshells secondaryGo;
	      (!Initialise.initialFn) true;
	      mainLoop sessionShell false
	    );
	in
	  mainGo ()
	end;
  in
    (* The main shell *)
    val (globalEnv, sessionShell) = 
      Initialise.initGlobalEnv 
         (rootShell, systemCommit, Debug.prompt1, Debug.prompt2);
       
    val U : unit =
      mainEnvShell := Some (globalEnv, sessionShell);
       
    val make = Make.makeIntoEnv globalEnv;
    val use  = Make.useIntoEnv globalEnv;
    
    fun shell () : unit = 
    let
      (* kill all db-specific values (no longer really necessary) *)
      val U : unit = CreateDatabase.resetSpecifics ();
    in
      (* install new compiler as main shell *)
      RunCall.run_call1 POLY_SYS_install_root rootShell
    end;
  end;
end;
