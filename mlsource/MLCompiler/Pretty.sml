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

    (* Pretty printer copied directly from basis/PrettyPrinter.  We can't use the
       same code because the "pretty" type is not the same. *)
    fun prettyPrint (stream : string -> unit, lineWidth : int) (pretty: pretty): unit =
    let
        fun printBlanks n =
            if n > 0 then (stream " "; printBlanks(n-1)) else ()

        (* Find out whether the block fits and return the space left if it does.
           Terminates with NONE as soon as it finds the line doesn't fit. *)
        fun getSize(PrettyBlock (_, _, _, entries), spaceLeft) =
            List.foldl(fn (p, SOME s) => getSize(p, s) | (_, NONE) => NONE)
                (SOME spaceLeft) entries

        |   getSize(PrettyBreak (blanks, _), spaceLeft) =
            if blanks <= spaceLeft then SOME(spaceLeft-blanks) else NONE

        |   getSize(PrettyString st, spaceLeft) =
            let
                val size = String.size st
            in
                if size <= spaceLeft
                then SOME(spaceLeft-size)
                else NONE
            end

        (* Lay out the block and return the space that is left after the line
           has been printed. *)
        fun layOut(p as PrettyBlock (blockOffset, consistent, _, entries), indent, spaceLeft) =
            let
                val blockIndent = indent+blockOffset
            in
                case getSize(p, spaceLeft) of
                    SOME s => (* Fits *)
                    (
                        (* Lay out the contents. This will not need to break. *)
                        List.foldl(fn(p, space) => layOut(p, blockIndent, space)) spaceLeft entries;
                        s
                    )
                |   NONE => (* Doesn't fit - break line somewhere. *)
                    let
                        (* Lay out this block, breaking where necessary. *)
                        fun doPrint([], left) = (* Finished: return what's left. *) left

                        |   doPrint([PrettyBreak _], left) =
                                left (* Ignore trailing breaks. *)

                        |   doPrint(PrettyBreak (blanks, breakOffset) :: rest, left) =
                            let
                                (* Compute the space of the next item(s) up to the end or the
                                   next space.  Since we only break at spaces if there are
                                   Blocks or Strings without spaces between we need to know
                                   the total size. *)
                                fun getsp([], left) = SOME left
                                |   getsp(PrettyBreak _ :: _, left) = SOME left
                                |   getsp(next::rest, left) =
                                        case getSize(next, left) of
                                            NONE => NONE
                                        |   SOME sp => getsp(rest, sp)
                            in
                                if consistent orelse not(isSome(getsp(rest, left)))
                                then (* Either a consistent break or the next item won't fit. *)
                                (
                                    stream "\n";
                                    printBlanks(blockIndent+breakOffset);
                                    doPrint(rest, lineWidth-blockIndent-breakOffset)
                                )
                                else (* We don't need to break here. *)
                                (
                                    printBlanks blanks;
                                    doPrint(rest, left-blanks)
                                )
                            end
 
                        |   doPrint(PrettyString s :: rest, left) =
                            (
                                stream s;
                                doPrint(rest, left-size s)
                            )

                        |   doPrint((b as PrettyBlock _) :: rest, left) =
                                doPrint(rest, layOut(b, blockIndent, left))

                        val onLine = doPrint(entries, spaceLeft);
                    in
                        onLine
                    end
            end
        |   layOut (PrettyBreak (blanks, _), _, spaceLeft) =
                ( printBlanks blanks; Int.max(spaceLeft-blanks, 0) )
        |   layOut (PrettyString st, _, spaceLeft) =
                ( stream st; Int.max(spaceLeft-String.size st, 0) )

    in
        if layOut(pretty, 0, lineWidth) <> lineWidth
        then stream "\n" (* End the line unless we haven't written anything. *)
        else ()
    end

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
