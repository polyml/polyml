(*
    Copyright (c) 2001, 2015
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

structure MenuBase =
struct
    local
        open Foreign Base
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
                (*(MF_INSERT,   0wx00000000),
                (MF_CHANGE,     0wx00000080),
                (MF_APPEND,     0wx00000100),
                (MF_DELETE,     0wx00000200),
                (MF_REMOVE,     0wx00001000),*)
                (MF_BYCOMMAND,  0wx00000000),
                (MF_BYPOSITION, 0wx00000400),
                (MF_SEPARATOR,  0wx00000800),
                (MF_ENABLED,    0wx00000000),
                (MF_GRAYED,     0wx00000001),
                (MF_DISABLED,   0wx00000002),
                (MF_UNCHECKED,  0wx00000000),
                (MF_CHECKED,    0wx00000008),
                (MF_USECHECKBITMAPS, 0wx00000200),
                (MF_STRING,     0wx00000000),
                (MF_BITMAP,     0wx00000004),
                (MF_OWNERDRAW,  0wx00000100),
                (MF_POPUP,      0wx00000010),
                (MF_MENUBARBREAK, 0wx00000020),
                (MF_MENUBREAK,  0wx00000040),
                (MF_UNHILITE,   0wx00000000),
                (MF_HILITE,     0wx00000080),
                (MF_DEFAULT,    0wx00001000),
                (MF_SYSMENU,    0wx00002000),
                (MF_HELP,       0wx00004000),
                (MF_RIGHTJUSTIFY, 0wx00004000),
                (MF_MOUSESELECT, 0wx00008000)
             ]
        in
            val (fromMenuFlagSet, toMenuFlagSet) = tableSetLookup(tab, NONE)
            val cMENUFLAGSET = absConversion {abs=toMenuFlagSet, rep=fromMenuFlagSet} cUintw
            (* Sometimes we just want a single flag - either MF_BYCOMMAND or MF_BYPOSITION
               or, for WM_MENUCHAR, MF_POPUP or MF_SYSMENU. *)
            val (fromMenuFlag, toMenuFlag) = tableLookup(tab, NONE)
            val cMENUFLAG = tableConversion(tab, NONE) cUintw
        end

    end
end;
