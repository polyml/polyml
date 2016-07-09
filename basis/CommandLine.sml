(*
    Title:      Standard Basis Library: CommandLine Structure and Signature
    Author:     David Matthews
    Copyright   David Matthews 1999, 2016

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


signature COMMAND_LINE =
sig
    val name : unit -> string
    val arguments : unit -> string list
end
     
structure CommandLine : COMMAND_LINE =
struct
    local
        val doCall: int * unit -> string = RunCall.rtsCallFull2 "PolyProcessEnvGeneral"
    in
        fun name() = doCall(0, ())
    end
    
    local
        val doCall: int * unit -> string list = RunCall.rtsCallFull2 "PolyProcessEnvGeneral"
    in
        fun arguments() = doCall(1, ())
    end
end;
