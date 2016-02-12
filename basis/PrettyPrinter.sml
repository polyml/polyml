(*
    Title:      Pretty Printer.
    Author:     David C. J. Matthews
    Copyright (c) 2009, 2013, 2015-16

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* This is a complete rewrite of the original pretty printer.  The original
   version was written in the Poly language in the early 80s and was based
   on the paper by D.C Oppen in ACM ToPLAS Vol. 2 No. 4 Oct 1980.

   His version was imperative and provided "blockBegin", "blockEnd",
   "break" and "string" functions to lay out the text.  This version uses
   the "pretty" datatype that uses the same model of the text but
   implements it completely differently.

   PrettyString s -
        Prints out the string s
   PrettyStringWithWidth (s, w) -
        The same as PrettyString except that the width is given explicitly
        rather than being the length of the string.  This is intended for
        multibyte encodings where the number of columns is not the same as
        the number of characters in the string
   PrettyBreak(blanks, offset) -
        Provides a place where the text may be broken.  If no break is needed it
        prints "blanks" spaces.  If a break is needed it add a temporary indent of
        "offset" to the indentation of the next block of text.
   PrettyBlock(indent, consistent, context, entries) -
        Defines a block of text.  If all the entries will fit on the line they
        are simply printed out.  If they do not fit the block must be broken.
        If "consistent" is true then every "PrettyBreak" within "entries" (at
        this level only, not recursively) is a break point and the line will be
        broken at that point.  If "consistent" is false then a line break is
        inserted at a PrettyBreak only if the following entry will not fit.
        "indent" is the value that is added to the effective indentation
        each time the line is broken within this block.  It does not affect
        the indentation of the first string in the block.
    PrettyLineBreak
        Insert an explicit line break
*)

(* N.B. The effect of compiling this file is to extend the PolyML structure. *)

structure PolyML =
struct
    open PolyML
    fun prettyMarkup (beginContext: context list -> unit, endContext: context list -> unit)
                     (stream : string -> unit, width: int) (pretty: pretty): unit =
    let
        val lineWidth = FixedInt.fromInt width

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
                val size = FixedInt.fromInt(String.size st)
            in
                if size <= spaceLeft
                then SOME(spaceLeft-size)
                else NONE
            end

        |   getSize(PrettyStringWithWidth(_, w), spaceLeft) =
                if w <= spaceLeft
                then SOME(spaceLeft-w)
                else NONE

        |   getSize(PrettyLineBreak, _) = NONE

        (* Lay out the block and return the space that is left after the line
           has been printed. *)
        fun layOut(p as PrettyBlock (blockOffset, consistent, context, entries), indent, spaceLeft) =
            let
                val blockIndent = indent+blockOffset
            in
                case getSize(p, spaceLeft) of
                    SOME s => (* Fits *)
                    (
                        beginContext context;
                        (* Lay out the contents. This will not need to break. *)
                        List.foldl(fn(p, space) => layOut(p, blockIndent, space)) spaceLeft entries;
                        endContext context;
                        s
                    )
                |   NONE => (* Doesn't fit - break line somewhere. *)
                    let
                        (* Lay out this block, breaking where necessary. *)
                        fun doPrint([], _, left) = (* Finished: return what's left. *) left

                        |   doPrint([PrettyBreak _], _, left) =
                                left (* Ignore trailing breaks. *)

                        |   doPrint(PrettyBreak (blanks, breakIndent) :: rest, _, left) =
                            let
                                val currentIndent = blockIndent+breakIndent
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
                                if consistent orelse left <= blanks orelse
                                    not(isSome(getsp(rest, left-blanks)))
                                then (* Either a consistent break or the next item won't fit. *)
                                (
                                    stream "\n";
                                    printBlanks currentIndent;
                                    (* Carry the indent associated with this item forward so
                                       that it is included in the block indentation if the next
                                       item is a block.  If it is a string we have already
                                       included it. *)
                                    doPrint(rest, breakIndent, lineWidth-currentIndent)
                                )
                                else (* We don't need to break here. *)
                                (
                                    printBlanks blanks;
                                    doPrint(rest, 0, left-blanks)
                                )
                            end
 
                        |   doPrint(PrettyString s :: rest, _, left) =
                            (
                                stream s;
                                doPrint(rest, 0, left - FixedInt.fromInt(size s))
                            )
 
                        |   doPrint(PrettyStringWithWidth(s, w) :: rest, _, left) =
                            (
                                stream s;
                                doPrint(rest, 0, left-w)
                            )

                        |   doPrint((b as PrettyBlock _) :: rest, breakIndent, left) =
                                doPrint(rest, 0, layOut(b, blockIndent+breakIndent, left))

                        |   doPrint(PrettyLineBreak :: rest, _, _) =
                            (
                                stream "\n";
                                printBlanks blockIndent;
                                doPrint(rest, 0, lineWidth-blockIndent)
                            )

                        val () = beginContext context;
                        val onLine = doPrint(entries, 0, spaceLeft);
                        val () = endContext context
                    in
                        onLine
                    end
            end

        |   layOut (PrettyBreak (blanks, _), _, spaceLeft) =
                ( printBlanks blanks; FixedInt.max(spaceLeft-blanks, 0) )

        |   layOut (PrettyString st, _, spaceLeft) =
                ( stream st; FixedInt.max(spaceLeft- FixedInt.fromInt(String.size st), 0) )

        |   layOut (PrettyStringWithWidth(st, w), _, spaceLeft) =
                ( stream st; FixedInt.max(spaceLeft-w, 0) )

        |   layOut (PrettyLineBreak, _, spaceLeft) =
                spaceLeft

    in
        if layOut(pretty, 0, lineWidth) <> lineWidth
        then stream "\n" (* End the line unless we haven't written anything. *)
        else ()
    end


    (* Basic pretty printer without mark-up of context. *)
    val prettyPrint = prettyMarkup (fn _ => (), fn _ => ())

end (* PolyML *);
