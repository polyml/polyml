(*
    Copyright (c) 2009 David C.J. Matthews

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

(* Signature for the pretty printer. *)
signature PRETTYSIG =
sig
    type context
    type pretty
    val ContextLocation:
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int } -> context
    and ContextProperty: string * string (* User property. *) -> context

    val PrettyBlock: int * bool * context list * pretty list -> pretty
    and PrettyBreak: int * int -> pretty
    and PrettyString: string -> pretty
    
    val isPrettyBlock: pretty -> bool
    val isPrettyBreak: pretty -> bool
    val isPrettyString: pretty -> bool
    
    val projPrettyBlock: pretty -> int * bool * context list * pretty list
    val projPrettyBreak: pretty -> int * int
    val projPrettyString: pretty -> string

(*
    datatype context =
        ContextLocation of
            { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
    |   ContextProperty of string * string (* User property. *)

    datatype pretty =
        PrettyBlock of int * bool * context list * pretty list
    |   PrettyBreak of int * int
    |   PrettyString of string
*)

    (* A simple "pretty printer" that just accumulates strings. *)
    val uglyPrint: pretty -> string

    (* A proper pretty printer. *)
    val prettyPrint: (string -> unit) * int -> pretty -> unit

    (* Tag for pretty printed out from PolyML.print. *)
    val printOutputTag : (pretty -> unit) Universal.tag
    (* Compiler output.  Used for timing information and compiler debug output. *)
    and compilerOutputTag: (pretty->unit) Universal.tag
    
    val getPrintOutput : Universal.universal list -> pretty -> unit
    and getCompilerOutput : Universal.universal list -> pretty -> unit
    (* getSimplePrinter prints strings through compilerOutput. *)
    and getSimplePrinter: Universal.universal list -> string -> unit

    (* Types that can be shared. *)
    structure Sharing:
    sig
        type pretty     = pretty
        and  context    = context
    end
end;
