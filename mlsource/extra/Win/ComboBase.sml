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

structure ComboBase =
struct
    local
        open Foreign Base
    in
        datatype CBDirAttr =
            DDL_READWRITE | DDL_READONLY | DDL_HIDDEN | DDL_SYSTEM | DDL_DIRECTORY |
            DDL_ARCHIVE | DDL_POSTMSGS | DDL_DRIVES | DDL_EXCLUSIVE
        local
            val tab = [
                (DDL_READWRITE, 0wx0000),
                (DDL_READONLY, 0wx0001),
                (DDL_HIDDEN, 0wx0002),
                (DDL_SYSTEM, 0wx0004),
                (DDL_DIRECTORY, 0wx0010),
                (DDL_ARCHIVE, 0wx0020),
                (DDL_POSTMSGS, 0wx2000),
                (DDL_DRIVES, 0wx4000),
                (DDL_EXCLUSIVE, 0wx8000)
                ]
        in
            val CBDIRATTRS = tableSetLookup(tab, NONE)
        end
    end
end;
