(*
    Title:      Standard Basis Library: NetServDB Structures and Signatures
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

signature NET_SERV_DB =
sig
    type entry
    val name : entry -> string
    val aliases : entry -> string list
    val port : entry -> int
    val protocol : entry -> string
    val getByName : string * string option -> entry option
    val getByPort : int * string option -> entry option
end;

structure NetServDB :> NET_SERV_DB =
struct
    type entry = string * string list * int * string

    val name: entry -> string = #1
    val aliases : entry -> string list = #2
    val port : entry -> int = #3
    val protocol : entry -> string = #4

    local
        val doCall1: int*string -> entry
             = RunCall.run_call2 RuntimeCalls.POLY_SYS_network
        and doCall2: int*(string*string) -> entry
             = RunCall.run_call2 RuntimeCalls.POLY_SYS_network
    in
        fun getByName(s, NONE) =
            ( SOME(doCall1(5, s)) handle _ => NONE )
         |  getByName(s, SOME p) =
            ( SOME(doCall2(6, (s, p))) handle _ => NONE )
    end

    local
        val doCall1: int*int -> entry
             = RunCall.run_call2 RuntimeCalls.POLY_SYS_network
        and doCall2: int*(int*string) -> entry
             = RunCall.run_call2 RuntimeCalls.POLY_SYS_network
    in
        fun getByPort(n, NONE) =
            ( SOME(doCall1(7, n)) handle _ => NONE )
         |  getByPort(n, SOME p) =
            ( SOME(doCall2(8, (n, p))) handle _ => NONE )
    end

end;
