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

(* DCJM June 2000.  Removed some functions which are no in the standard
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

	(* These are retained for backwards compatibility.  It's quite
	   likely that people will use them interactively so a message is
	   quite useful. *)
    fun exit (n: int) : unit =
		(
		(!RunCall.printString) "The standard function is OS.Process.exit\n";
		RunCall.run_call1 POLY_SYS_exit (n mod 256)
		)

    fun quit() = exit 0;

	local
		val chDir: string -> unit = RunCall.run_call1 POLY_SYS_chdir
	in
		fun cd s =
			(
			(!RunCall.printString) "The standard function is OS.FileSys.chDir\n";
			chDir s
			)
	end

	local
		val etrace: word->word = RunCall.run_call1 POLY_SYS_exception_trace;
	in
		fun exception_trace (f: unit->'a):'a =
			RunCall.unsafeCast etrace f
	end

	local
		(* Each database has its own list of start-up functions. *)
		type t = ((unit->unit) list) ref
		val onEntryList: t RunCall.dbList =
			RunCall.dbListCreate (fn () => (ref[] : t),
								  [ref[(! RunCall.startupFn)]]: t list) 
		(* Run the list in reverse order - First the parent database
		   in reverse order then the other databases. *)
		fun runOnEntry [] = ()
		  | runOnEntry (f :: b) = (runOnEntry b; f() handle _ => ());
		fun runOnEntries [] = ()
		  | runOnEntries (a :: b) = (runOnEntries b; runOnEntry(!a));
		(* Set the start-up function to run this list. *)
		val unused = RunCall.startupFn :=
				(fn () => runOnEntries(RunCall.dbListToList onEntryList))
	in
		(* The equivalent of atExit except that functions are added to
		   the list persistently and of course the functions are executed
		   at start-up rather than close-down. *)
		fun onEntry (f: unit->unit) =
			case RunCall.dbListToList onEntryList of
				[] => raise Fail "onEntry - no list"
			|	(l :: _) => l := f :: !l
	end
  end;
end;
