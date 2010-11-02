(*
    Title:      Standard Basis Library: NetProtDB Structures and Signatures
    Author:     David Matthews
    Copyright   David Matthews 2000

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

(* G&R 2004 status: checked, no change. *)

signature NET_PROT_DB =
sig
    type entry
    val name : entry -> string
    val aliases : entry -> string list
    val protocol : entry -> int
    val getByName : string -> entry option
    val getByNumber : int -> entry option
end;

structure NetProtDB :> NET_PROT_DB =
struct
    type entry = string * string list * int

    val name: entry -> string = #1
    val aliases : entry -> string list = #2
    val protocol : entry -> int = #3

    local
        val doCall: int*string -> entry
             = RunCall.run_call2 RuntimeCalls.POLY_SYS_network
    in
        fun getByName s =
            SOME(doCall(3, s)) handle OS.SysErr _ => NONE
    end

    local
        val doCall: int*int -> entry
             = RunCall.run_call2 RuntimeCalls.POLY_SYS_network
    in
        fun getByNumber n =
            SOME(doCall(4, n)) handle OS.SysErr _ => NONE
    end

end;
