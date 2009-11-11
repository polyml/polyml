(*
    Title:      Standard Basis Library: BIT_FLAGS signature
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

signature BIT_FLAGS =
sig
    eqtype flags
    val toWord   : flags -> SysWord.word
    val fromWord : SysWord.word -> flags
    val all: flags
    val flags : flags list -> flags
    val intersect: flags list -> flags
    val clear: flags * flags -> flags
    val allSet : flags * flags -> bool
    val anySet : flags * flags -> bool
end;
