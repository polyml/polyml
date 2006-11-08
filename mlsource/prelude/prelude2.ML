(*
    Title: 	Secondary ML prelude file.
    Author: 	Dave Matthews, Cambridge University Computer Laboratory
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

(* DCJM June 2000.  Removed some functions which are not in the standard
   basis library and added onEntry *)
local
    open RuntimeCalls;
in

    structure PolyML =
    (* We must not have a signature on the result otherwise print and makestring
       will be given polymorphic types and will only produce "?" *)

    struct
    (* Some of these are inherited from PolyML in the root environment, but
       this structure hides that one. *)
    open PolyML.Inner;

	local
	    (* This is slightly awkward but is needed because otherwise the compiler
		   would complain about a free type variable. *)
		val etrace: word->word = RunCall.run_call1 POLY_SYS_exception_trace;
	in
		fun exception_trace (f: unit->'a):'a =
			RunCall.unsafeCast etrace f
	end

	local
	    (* Initialise the list with the existing start-up function *)
		val onEntryList: (unit->unit) list ref = ref[]
		(* Run the list in reverse order. *)
		fun runOnEntry [] = ()
		  | runOnEntry (f :: b) = (runOnEntry b; f() handle _ => ());
	in
		(* The equivalent of atExit except that functions are added to
		   the list persistently and of course the functions are executed
		   at start-up rather than close-down. *)
		fun onEntry (f: unit->unit) = onEntryList := f :: !onEntryList
		
    	(* Export functions - write out the function and everything reachable from it. *)
    	fun export(filename: string, f: unit->unit): unit =
		    let
		       fun run () = (runOnEntry(! onEntryList); f())
			in 
    		   RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (1, (filename, run))
			end
    	fun exportPortable(filename: string, f: unit->unit): unit =
		    let
		       fun run () = (runOnEntry(! onEntryList); f())
			in 
     		   RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (3, (filename, run))
			end
	end
		
	fun shareCommonData(root: 'a): unit =
		RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (13, root)


    (* The first two used to be in the System structure but that is no longer included. *)
  	fun objSize(x:'a): int    = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (14, x)
	and showSize(x:'a): int   = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (15, x)
	and objProfile(x:'a): int = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (16, x)
	
	val fullGC: unit -> unit = 
	    RunCall.run_call0 POLY_SYS_full_gc;
	
	val stackTrace: unit -> unit = 
	    RunCall.run_call0 POLY_SYS_stack_trace;
	  
	local
		val eqWord : word*word->bool = RunCall.run_call2 POLY_SYS_word_eq
    in
		fun pointerEq(x: 'a ,y: 'a): bool = RunCall.unsafeCast eqWord (x,y)
	end
	
	fun rtsVersion () : int = RunCall.run_call2 POLY_SYS_process_env(104, ())
	fun architecture(): string = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (12, ())
    fun rtsArgumentHelp(): string = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (19, ());

    end;
end;
