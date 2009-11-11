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

structure MenuBase =
struct
    local
        open CInterface Base
    in
        (* TODO: This duplicates some of the other datatypes. *)
        datatype MenuFlag =
            (*MF_INSERT | MF_CHANGE | MF_APPEND | MF_DELETE | MF_REMOVE | *)
            MF_BYCOMMAND | MF_BYPOSITION | MF_SEPARATOR | MF_ENABLED | MF_GRAYED |
            MF_DISABLED | MF_UNCHECKED | MF_CHECKED | MF_USECHECKBITMAPS | MF_STRING |
            MF_BITMAP | MF_OWNERDRAW | MF_POPUP | MF_MENUBARBREAK | MF_MENUBREAK |
            MF_UNHILITE | MF_HILITE | MF_DEFAULT | MF_SYSMENU | MF_HELP |
            MF_RIGHTJUSTIFY | MF_MOUSESELECT

        local
            val tab = [
                (*(MF_INSERT,   0x00000000),
                (MF_CHANGE,     0x00000080),
                (MF_APPEND,     0x00000100),
                (MF_DELETE,     0x00000200),
                (MF_REMOVE,     0x00001000),*)
                (MF_BYCOMMAND,  0x00000000),
                (MF_BYPOSITION, 0x00000400),
                (MF_SEPARATOR,  0x00000800),
                (MF_ENABLED,    0x00000000),
                (MF_GRAYED,     0x00000001),
                (MF_DISABLED,   0x00000002),
                (MF_UNCHECKED,  0x00000000),
                (MF_CHECKED,    0x00000008),
                (MF_USECHECKBITMAPS, 0x00000200),
                (MF_STRING,     0x00000000),
                (MF_BITMAP,     0x00000004),
                (MF_OWNERDRAW,  0x00000100),
                (MF_POPUP,      0x00000010),
                (MF_MENUBARBREAK, 0x00000020),
                (MF_MENUBREAK,  0x00000040),
                (MF_UNHILITE,   0x00000000),
                (MF_HILITE,     0x00000080),
                (MF_DEFAULT,    0x00001000),
                (MF_SYSMENU,    0x00002000),
                (MF_HELP,       0x00004000),
                (MF_RIGHTJUSTIFY, 0x00004000),
                (MF_MOUSESELECT, 0x00008000)
             ]
        in
            val (fromMenuFlagSet, toMenuFlagSet) = tableSetLookup(tab, NONE)
            val MENUFLAGSET = absConversion {abs=toMenuFlagSet, rep=fromMenuFlagSet} INT
            (* Sometimes we just want a single flag - either MF_BYCOMMAND or MF_BYPOSITION
               or, for WM_MENUCHAR, MF_POPUP or MF_SYSMENU. *)
            val (fromMenuFlag, toMenuFlag) = tableLookup(tab, NONE)
            val MENUFLAG = tableConversion(tab, NONE)
        end

    end
end;
