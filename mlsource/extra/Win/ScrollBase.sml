(*
    Copyright (c) 2001
        David C.J. Matthews

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

structure ScrollBase =
struct
    local
        open CInterface Base
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
                IntInf.orb(if enableLeftUp then 0 else 1,
                           if enableRightDown then 0 else 2)
            and fromInt i : enableArrows =
                {enableLeftUp = IntInf.andb(i, 1) = 0,
                 enableRightDown = IntInf.andb(i, 2) = 0} 
        in
            val ENABLESCROLLBARFLAG = absConversion{rep = toInt, abs = fromInt} INT
        end

        type SCROLLINFO =
            { minPos: int, maxPos: int, pageSize: int, pos: int, trackPos: int }

        datatype ScrollInfoOption =
            SIF_RANGE | SIF_PAGE | SIF_POS | SIF_DISABLENOSCROLL | SIF_TRACKPOS

        val SIF_ALL = [SIF_RANGE, SIF_PAGE, SIF_POS, SIF_TRACKPOS]

        local
            val tab = [
                (SIF_RANGE,           0x0001),
                (SIF_PAGE,            0x0002),
                (SIF_POS,             0x0004),
                (SIF_DISABLENOSCROLL, 0x0008),
                (SIF_TRACKPOS,        0x0010)]
        in
            (*val (fromSIF, toSIF) = tableSetLookup(tab, NONE)*)
            val SCROLLINFOOPTION = tableSetConversion(tab, NONE)
        end

        local
            val SCROLLINFOSTRUCT = STRUCT7(INT, SCROLLINFOOPTION, INT, INT, INT, INT, INT)
            val (_, _, scrollInfoStruct) = breakConversion SCROLLINFOSTRUCT

            fun fromStruct(_, mask, minPos, maxPos, pageSize, pos, trackPos) =
                ({minPos = minPos, maxPos = maxPos, pageSize = pageSize,
                 pos = pos, trackPos = trackPos}, mask)
            and toStruct({ minPos, maxPos, pageSize, pos, trackPos}, options) =
                (sizeof scrollInfoStruct, options, minPos, maxPos, pageSize, pos, trackPos)
        in
            val SCROLLINFO = absConversion{abs=fromStruct, rep=toStruct} SCROLLINFOSTRUCT
        end
    end
end;
