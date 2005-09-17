(*
    Title:      Process package for ML.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
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

local
  open RuntimeCalls;
  (* open Address; *)
  val F_mutable_words = 64;
in

structure Process:
sig
   type 'a channel 
   val channel: unit -> '_a channel
   val send:    'a channel * 'a -> unit
   val receive: 'a channel -> 'a
   val fork:    (unit->unit)->unit
   val console: (unit->unit)->(unit->unit)
   val choice:  (unit->unit)*(unit->unit)->unit
   val interruptConsoleProcesses: unit->unit
end =

struct
   (* A typed communications channel with no buffering.
       This should be constrained to be a monotype. *)
   abstype 'a channel =
      dummy of int (* Handled entirely by run-time system. *)
   with
     (* Create a channel represented by a two-word *mutable* vector. *)
     fun channel () : '_a channel =
	 	RunCall.run_call3 POLY_SYS_alloc_store(2, F_mutable_words, 0)
             
     (* Send a value on a channel. *)
     fun send (chann : 'a channel, value : 'a) : unit =
       (RunCall.run_call2 POLY_SYS_send_on_channel (chann, value); ());
            
     (* Receive a value on a channel. *)
    fun receive (chann : 'a channel) : 'a =
      RunCall.run_call1 POLY_SYS_receive_on_channel chann;
  end;

  (* Fork a non-interruptable process. *)
  fun fork (f : unit->unit) : unit =
      (RunCall.run_call2 POLY_SYS_fork_process (f, 0); ());

  (* Fork an interruptable process. *)
  fun console (f : unit->unit) : unit -> unit =
  let
    (* Fork the process. *)
    val pid = RunCall.run_call2 POLY_SYS_fork_process (f, 1);
  in
    (* Return the interrupter. *)
    fn () => (RunCall.run_call1 POLY_SYS_int_process pid; ())
  end;

   (* Fork two processes. Only one is allowed to perform a transfer. *)
  fun choice (a : unit->unit, b : unit->unit) : unit =
      (RunCall.run_call2 POLY_SYS_choice_process (a, b); ())

  (* interruptConsoleProcesses: interrupt all console processes.
     Added DCJM 1/9/00.  This function was originally in the System
	 structure. *)
   val interruptConsoleProcesses: unit->unit = 
   		RunCall.run_call0 POLY_SYS_interrupt_console_processes;
end;
end; (* open RuntimeCalls *)
