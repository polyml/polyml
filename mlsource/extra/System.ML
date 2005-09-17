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

local
  open RuntimeCalls;
in

structure System =
struct

  local
  	 val osize: word->int = RunCall.run_call1 POLY_SYS_objsize
	 and ssize: word->int = RunCall.run_call1 POLY_SYS_showsize
  in
  	fun objsize x = osize(RunCall.unsafeCast x)
	and showsize x = ssize(RunCall.unsafeCast x)
  end

  val interrupt_console_processes: unit -> unit = 
    RunCall.run_call0 POLY_SYS_interrupt_console_processes;

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
end;

end; (* open RuntimeCalls *)
