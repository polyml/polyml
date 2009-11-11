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
    |   uglyPrint(PrettyBreak(n, _)) = String.implode(List.tabulate(n, fn _ => #" "))
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
        
        (* The low-level code-generators print strings a bit at a time and separate the lines
           with new-line characters.  This provides a simple string printer for backwards
           compatibility. *)
        fun getSimplePrinter parameters =
        let
            val compilerOut: pretty -> unit = getTag compilerOutputTag parameters
            val buff = ref ""
            fun printStream (s: string) =
            let
                (* If there's a newline split there. *)
                val (a, b) = Substring.splitl(fn #"\n" => false | _ => true) (Substring.full(!buff ^ s))
            in
                if Substring.size b = 0 (* No newline. *)
                then buff := Substring.string a
                else
                (
                    compilerOut(PrettyString(Substring.string a));
                    buff := "";
                    printStream(Substring.string(Substring.slice(b, 1, NONE))) 
                )
            end
        in
            printStream
        end
    end

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type pretty     = pretty
        and  context    = context
    end
end;
