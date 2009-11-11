(*
    Title:      Standard Basis Library: CommandLine Structure and Signature
    Author:     David Matthews
    Copyright   David Matthews 1999

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

(* G&R 2004 checked: no change. *)


signature COMMAND_LINE =
sig
    val name : unit -> string
    val arguments : unit -> string list
end
     
structure CommandLine : COMMAND_LINE =
struct
    fun callProcEnv (code: int) args =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_process_env (code,args);

    val name : unit -> string = callProcEnv 0
    val arguments : unit -> string list = callProcEnv 1
end;
