(*
    Title:      Standard Basis Library: Internet Sockets
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

(* G&R 2004 status: Checked, no change. *)

signature INET_SOCK =
sig
    type inet

    type 'sock_type sock = (inet, 'sock_type) Socket.sock
    type 'mode stream_sock = 'mode Socket.stream sock

    type dgram_sock = Socket.dgram sock
    type sock_addr = inet Socket.sock_addr

    val inetAF : Socket.AF.addr_family
    val toAddr : NetHostDB.in_addr * int -> sock_addr
    val fromAddr : sock_addr -> NetHostDB.in_addr * int
    val any : int -> sock_addr
    structure UDP :
    sig
        val socket : unit -> dgram_sock
        val socket' : int -> dgram_sock
    end
    structure TCP :
    sig
        val socket : unit -> 'mode stream_sock
        val socket' : int -> 'mode stream_sock
        val getNODELAY : 'mode stream_sock -> bool
        val setNODELAY : 'mode stream_sock * bool -> unit
    end
end;

structure INetSock :> INET_SOCK =
struct
    abstype inet = ABSTRACT with end;

    type 'sock_type sock = (inet, 'sock_type) Socket.sock
    type 'mode stream_sock = 'mode Socket.stream sock

    type dgram_sock = Socket.dgram sock
    type sock_addr = inet Socket.sock_addr

    val inetAF : Socket.AF.addr_family =
        case Socket.AF.fromString "INET" of
            NONE => raise OS.SysErr("Missing address family", NONE)
        |   SOME s => s

    local
        val doCall = RunCall.run_call2 RuntimeCalls.POLY_SYS_network
    in
        fun toAddr(iaddr: NetHostDB.in_addr, port: int) : sock_addr =
            doCall(40, (port, iaddr))
    end

    local
        val doCall1 = RunCall.run_call2 RuntimeCalls.POLY_SYS_network
        and doCall2 = RunCall.run_call2 RuntimeCalls.POLY_SYS_network
    in
        fun fromAddr (s: sock_addr) : NetHostDB.in_addr * int =
            if Socket.familyOfAddr s <> inetAF
            then raise Match
            else (doCall1(42, s), doCall2(41, s))
    end

    local
        val doCall = RunCall.run_call2 RuntimeCalls.POLY_SYS_network
        val iAddrAny: NetHostDB.in_addr = doCall(13, ())
    in
    fun any (p: int) : sock_addr = toAddr(iAddrAny, p)
    end
    
    local
        val doCall1 = RunCall.run_call2 RuntimeCalls.POLY_SYS_network
        val doCall2 = RunCall.run_call2 RuntimeCalls.POLY_SYS_network
    in
        structure UDP =
        struct
            fun socket () = GenericSock.socket(inetAF, Socket.SOCK.dgram)
            fun socket' p = GenericSock.socket'(inetAF, Socket.SOCK.dgram, p)
        end

        structure TCP =
        struct
            fun socket () = GenericSock.socket(inetAF, Socket.SOCK.stream)
            fun socket' p = GenericSock.socket'(inetAF, Socket.SOCK.stream, p)

            fun getNODELAY(s: 'mode stream_sock): bool =
                doCall1(16, RunCall.unsafeCast s)

            fun setNODELAY (s: 'mode stream_sock, b: bool): unit =
                doCall2(15, (RunCall.unsafeCast  s, b))
        end
    end 

end;
