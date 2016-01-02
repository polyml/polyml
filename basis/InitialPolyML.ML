(*
    Title:  Extend the PolyML structure.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

    Modified David C.J. Matthews 2008, 2015

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* Extend the PolyML structure.  In particular this adds onEntry which needs to
   be used further on in the basis library.  We also add a few more items at
   this point. *)

local
    open RuntimeCalls
in

    structure PolyML =
    (* We must not have a signature on the result otherwise print and makestring
       will be given polymorphic types and will only produce "?" *)

    struct
        open PolyML

        local
            (* Initialise the list with the existing start-up function. *)
            val onEntryList: (unit->unit) list ref = ref[]
            and onEntryMutex = Thread.Mutex.mutex()

            (* Run the list in reverse order. *)
            fun runOnEntry [] = ()
              | runOnEntry (f :: b) = (runOnEntry b; f() handle _ => ());

            (* This wraps the function provided to PolyML.export and PolyML.exportPortable
               so that the library is initialised at start-up and finalised at close-down. *)
            fun runFunction f () =
            let
                val () = runOnEntry(! onEntryList); (* Perform start-up operations. *)
                (* Run the main program.  If it doesn't explicitly call OS.Process.exit then
                   use "success" as the normal result and "failure" if it raises an exception. *)
                val result = (f(); OS.Process.success) handle _ => OS.Process.failure (* Run the main function. *)
            in
                OS.Process.exit result (* Perform close-down actions. *)
            end
        in
            (* The equivalent of atExit except that functions are added to
               the list persistently and of course the functions are executed
               at start-up rather than close-down. *)
            (* Protect this with a mutex in case two threads try to add entries at the
               same time.  Very unlikely since this is really only called when building
               the basis library. *)
            fun onEntry (f: unit->unit) : unit =
                ThreadLib.protect onEntryMutex (fn () => onEntryList := f :: !onEntryList) ()
        
            (* Export functions - write out the function and everything reachable from it. *)
            fun export(filename: string, f: unit->unit): unit =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (1, (filename, runFunction f))
            fun exportPortable(filename: string, f: unit->unit): unit =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (3, (filename, runFunction f))
        end
        
        fun shareCommonData(root: 'a): unit =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (13, root)

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

        structure IntInf =
        struct
            fun gcd(args: int * int): int =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (50, args)
            and lcm(args: int * int): int =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (51, args)
        end
    end

end;
