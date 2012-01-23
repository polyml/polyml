(*
    Title:  PolyML structure before compiling the rest of the basis.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

    Modified David C.J. Matthews 2008

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

(* We need to use onEntry in the IO library so this has to be compiled
   first.  However we also want "make" in the PolyML structure so we have
   to complete the compilation later. *)
local
    open RuntimeCalls;
in

    structure PolyML =
    (* We must not have a signature on the result otherwise print and makestring
       will be given polymorphic types and will only produce "?" *)

    struct
    (* The type-specific functions e.g. PolyML.print must be extracted by using open.
       They appear to be polymorphic but we have to be careful that they are
       recognised by the compiler as type-specific and not as normal polymorphic
       functions. *)
    open PolyML;

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

        (* If the installed function returns without calling OS.Process.exit
           we have to run the installed "atExit" functions as though it had.
           Unfortunately we have to duplicate code from OS.Process here. *)
        local
            val doExit =
                RunCall.run_call1 RuntimeCalls.POLY_SYS_exit
            val doCall: int*unit -> (unit->unit) =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_process_env
        in
            fun exit () =
            let
                val exitFun =
                    (* If we get an empty list here we've finished. *)
                    doCall(19, ()) handle _ => doExit 0
            in
                (* Run the function and then repeat. *)
                exitFun() handle _ => ();
                exit()
            end
        end
    in
        (* The equivalent of atExit except that functions are added to
           the list persistently and of course the functions are executed
           at start-up rather than close-down. *)
        (* TODO: There ought to be a mutex here. *)
        fun onEntry (f: unit->unit) = onEntryList := f :: !onEntryList

        fun runFunction f () =
        (
            runOnEntry(! onEntryList); (* Perform start-up operations. *)
            f(); (* Run the main function. *)
            exit() (* Perform close-down actions. *)
        )
        
        (* Export functions - write out the function and everything reachable from it. *)
        fun export(filename: string, f: unit->unit): unit =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (1, (filename, runFunction f))
        fun exportPortable(filename: string, f: unit->unit): unit =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (3, (filename, runFunction f))
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

    local
        val onLoadList = ref []
    in
        (* Add a function to be called when a saved state is loaded.  This is really
           here to ensure that we can preserve the contents of the buffer for stdIn. *)
        fun onLoad (f: (unit->unit)->unit): unit = onLoadList := f :: ! onLoadList
    
        (* Saving and loading state. *)
        structure SaveState = 
            struct
            fun saveChild(f: string, depth: int): unit =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (20, (f, depth))
            fun saveState f = saveChild (f, 0);
            fun showHierarchy(): string list =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (22, ())
            fun renameParent{ child: string, newParent: string }: unit =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (23, (child, newParent))
            fun showParent(child: string): string option =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (24, child)

            fun loadState (f: string): unit =
            let
                val loadList = ! onLoadList
                (* Work along the list calling each function with an argument to
                   call the next item.  That allows the functions to make local copies
                   of any references and then reset them when the recursion unwinds. *)
                fun apply [] = (* Actually do the loading. *)
                    RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (21, f)
                |   apply (h::t) = h (fn () => apply t)
            in
                apply loadList;
                (* Reset the load list since that will have been overwritten by the load. *)
                onLoadList := loadList
            end
        end
    end

    (* Raise an exception using a given location rather than the value in the packet. *)
    fun raiseWithLocation(ex: exn, {file, startLine, startPosition, endLine, endPosition}: location) =
    let
        open RunCall RuntimeCalls
        fun getEntry n = run_call2 POLY_SYS_load_word(ex, n)
        (* This datatype is used in VALUE_OPS and FinalPolyML to define the format of a
           location in an exception packet.  It includes the possibility that the location
           information may be missing. *)
        datatype RuntimeLocation =
            NoLocation
        |   SomeLocation of
                (* file: *) string * 
                (*startLine:*) int *  (*startPosition:*) int *
                (*endLine:*) int * (*endPosition:*) int
       val packet =
        (getEntry 0, getEntry 1, getEntry 2,
                SomeLocation(file, startLine, startPosition, endLine, endPosition))
    in
        run_call1 POLY_SYS_raisex packet
    end


    fun exceptionLocation(exn: exn): location option =
    let
        open RuntimeCalls
        datatype RuntimeLocation =
            NoLocation
        |   SomeLocation of
                (* file: *) string * 
                (*startLine:*) int *  (*startPosition:*) int *
                (*endLine:*) int * (*endPosition:*) int
    in
        (* If we get an exception in the compiler it may be code that was built using the
           old exception packet format that didn't include a loction so we need to check the
           length of the packet first.  This won't be needed once we can be sure we're using
           5.3. *)
        if RunCall.run_call1 POLY_SYS_get_length exn < 0w4
        then NONE
        else case RunCall.run_call2 POLY_SYS_load_word(exn, 0w3) of
            NoLocation => NONE
        |   SomeLocation(file, startLine, startPosition, endLine, endPosition) =>
                SOME { file=file, startLine=startLine, startPosition=startPosition,
                       endLine=endLine, endPosition=endPosition }
    end
    
        exception XWindows = RunCall.XWindows
        and       Foreign  = RunCall.Foreign

        structure IntInf =
        struct
            fun gcd(args: int * int): int =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (50, args)
            and lcm(args: int * int): int =
                RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (51, args)
        end
    end

end;
