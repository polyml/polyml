(*
	Copyright (c) 2009 David C.J. Matthews 2008.

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

(* Datatype for pretty printing. *)

structure Pretty: PRETTYSIG =
struct
    datatype context =
        ContextLocation of
            { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
    |   ContextParentStructure of string * context list
    |   ContextProperty of string * string (* User property. *)

    datatype pretty =
        PrettyBlock of int * bool * context list * pretty list
    |   PrettyBreak of int * int
    |   PrettyString of string
    
    
    fun uglyPrint(PrettyBlock(_,_,_,l)) = String.concat(map uglyPrint l)
    |   uglyPrint(PrettyBreak _) = " "
    |   uglyPrint(PrettyString s) = s

    local
        open Universal
    in
        (* Tag for pretty printed out from PolyML.print. *)
        val printOutputTag : (pretty -> unit) tag = tag()
        (* Compiler output.  Used for timing information and compiler debug output. *)
        and compilerOutputTag: (pretty->unit) tag = tag()
    end

    local
        open Universal
        fun getTag (t: (pretty -> unit) tag) (tagList: universal list) : pretty -> unit =
            case List.find (tagIs t) tagList of
                SOME a => tagProject t a
            |   NONE => fn _ => () (* Use the default *)
    in
        val getPrintOutput = getTag printOutputTag
        and getCompilerOutput = getTag compilerOutputTag
    end

end;
