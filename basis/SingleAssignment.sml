(*
    Title:      References that allow a single assignment
    Author:     David Matthews
    Copyright   David Matthews 2010

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

structure SingleAssignment:>
sig
    type 'a saref (* Equality not allowed *)
    exception Locked
    val saref: unit -> 'a saref
    val savalue: 'a saref -> 'a option
    val saset: 'a saref * 'a -> unit
end
=
struct
    exception Locked

    type 'a saref = 'a option ref

    fun saref () = ref NONE
    
    val savalue = !

    fun saset(saVar as ref NONE, newValue) =
    let
        val lock = RunCall.run_call1 RuntimeCalls.POLY_SYS_lockseg
    in
        saVar := SOME newValue;
        lock saVar
    end
    |   saset _ = raise Locked
end;
