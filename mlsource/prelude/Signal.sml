(*
    Title:      Signal structure and signature.
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
signature SIGNAL =
sig
	datatype sig_handle = SIG_DFL | SIG_IGN | SIG_HANDLE of int->unit
	val signal: int * sig_handle -> sig_handle
end;

structure Signal: SIGNAL =
struct
	datatype sig_handle = SIG_DFL | SIG_IGN | SIG_HANDLE of int->unit
	local
		val doSig = RunCall.run_call2 RuntimeCalls.POLY_SYS_signal_handler
	in
		fun signal(s, cmd) =
		let
			val c =
				case cmd of
					SIG_DFL => 0
				|	SIG_IGN => 1
				|	SIG_HANDLE f => RunCall.unsafeCast f
		in
			case doSig(0, (s, c)) of
				0 => SIG_DFL
			|	1 => SIG_IGN
			|	f => SIG_HANDLE(RunCall.unsafeCast f)
		end
	end
end;
