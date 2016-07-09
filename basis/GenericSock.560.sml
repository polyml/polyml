(*
    Title:      Standard Basis Library: Generic socket structure and signature.
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

signature GENERIC_SOCK =
sig
    val socket : Socket.AF.addr_family * Socket.SOCK.sock_type
                    -> ('af, 'sock_type) Socket.sock
    val socketPair : Socket.AF.addr_family
                        * Socket.SOCK.sock_type
                        -> ('af, 'sock_type) Socket.sock
                        * ('af, 'sock_type) Socket.sock
    val socket' : Socket.AF.addr_family
                     * Socket.SOCK.sock_type
                     * int -> ('af, 'sock_type) Socket.sock
    val socketPair' : Socket.AF.addr_family
                         * Socket.SOCK.sock_type
                         * int
                         -> ('af, 'sock_type) Socket.sock
                         * ('af, 'sock_type) Socket.sock
end;

structure GenericSock : GENERIC_SOCK =
struct
    fun socket' (af, st, p: int) =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_network(14, (af, st, p))
    fun socketPair' (af, st, p: int) =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_network(55, (af, st, p))
    (* We assume that the default protocol is always zero. *)
    fun socket(af, st) = socket'(af, st, 0)
    fun socketPair(af, st) = socketPair'(af, st, 0)
end;

