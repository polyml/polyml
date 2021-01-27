(*
    Copyright (c) 2021 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.
    
    You should have received a copy of the GNU Lesser General Public
    Licence along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

signature Arm64Assembly =
sig
    type code
    type closureRef
    
    (* Create a code value for the function. *)
    val codeCreate: string * Universal.universal list -> code
    
    
    (* copyCode - create the vector of code and update the closure reference to
       point to it. *)
    val generateCode: {code: code, maxStack: int, resultClosure: closureRef} -> unit

    exception Fallback (* During development only. *)

    structure Sharing:
    sig
        type code = code
        type closureRef = closureRef
    end
end;
