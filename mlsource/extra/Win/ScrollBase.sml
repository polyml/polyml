(*
    Copyright (c) 2001, 2015, 2019
        David C.J. Matthews

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

structure ScrollBase =
struct
    local
        open Foreign Base
    in
        type enableArrows = { enableLeftUp: bool, enableRightDown: bool }
        val ESB_ENABLE_BOTH = { enableLeftUp = true, enableRightDown = true }
        val ESB_DISABLE_BOTH = { enableLeftUp = false, enableRightDown = false }
        val ESB_DISABLE_LEFT = { enableLeftUp = false, enableRightDown = true }
        val ESB_DISABLE_RIGHT = { enableLeftUp = true, enableRightDown = false }
        val ESB_DISABLE_UP = ESB_DISABLE_LEFT
        val ESB_DISABLE_DOWN = ESB_DISABLE_RIGHT

        local
            (* The arrows are disabled if the bit is set. *)
            fun toInt({enableLeftUp: bool, enableRightDown}: enableArrows) =
                Word.toInt(
                    Word.orb(if enableLeftUp then 0w0 else 0w1,
                           if enableRightDown then 0w0 else 0w2))
            and fromInt i : enableArrows =
                {enableLeftUp = Word.andb(Word.fromInt i, 0w1) = 0w0,
                 enableRightDown = Word.andb(Word.fromInt i, 0w2) = 0w0} 
        in
            (* It's easier to use the functions directly for messages *)
            val ENABLESCROLLBARFLAG = (toInt, fromInt)
            val cENABLESCROLLBARFLAG = absConversion{rep = toInt, abs = fromInt} cUint
        end

        type SCROLLINFO =
            { minPos: int, maxPos: int, pageSize: int, pos: int, trackPos: int }

        datatype ScrollInfoOption =
            SIF_RANGE | SIF_PAGE | SIF_POS | SIF_DISABLENOSCROLL | SIF_TRACKPOS

        val SIF_ALL = [SIF_RANGE, SIF_PAGE, SIF_POS, SIF_TRACKPOS]

        local
            val tab = [
                (SIF_RANGE,           0wx0001),
                (SIF_PAGE,            0wx0002),
                (SIF_POS,             0wx0004),
                (SIF_DISABLENOSCROLL, 0wx0008),
                (SIF_TRACKPOS,        0wx0010)]
        in
            (*val (fromSIF, toSIF) = tableSetLookup(tab, NONE)*)
            val cSCROLLINFOOPTION = tableSetConversion(tab, NONE)
        end

        (* Needed in Scrollbar and also Messages *)
        val cSCROLLINFOSTRUCT =
            cStruct7(cUint, cSCROLLINFOOPTION, cInt, cInt, cUint, cInt, cInt)
    end
end;
