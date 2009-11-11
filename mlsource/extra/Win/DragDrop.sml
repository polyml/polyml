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
structure DragDrop:
  sig
    type HDROP
    type HWND (* = Window.HWND *)
    type POINT = { x: int, y: int }
    val DragAcceptFiles : HWND * bool -> unit
    val DragFinish : HDROP -> unit
    val DragQueryFile : HDROP -> string list
    val DragQueryPoint : HDROP -> POINT * bool
  end =
struct
    local
        open CInterface Base
    in
        type HDROP = HDROP and HWND = HWND
        type POINT = POINT

        (* Call DragAcceptFiles to accept files. *)
        val DragAcceptFiles = call2 (shell "DragAcceptFiles") (HWND,BOOL) VOID

        (* Call DragFinish when finished processing a WM_DROP message. *)
        and DragFinish = call1 (shell "DragFinish") (HDROP) VOID

        (* Call DragQueryFile to get the file(s). *)
        local
            val dragQueryFile = call4 (shell "DragQueryFileA") (HDROP,INT,POINTER,UINT) UINT
        in
            fun DragQueryFile (hd: HDROP): string list =
            let
                val nfiles = dragQueryFile(hd, ~1, toCint 0, 0)
                fun getFile n =
                let
                    val buffsize =
                        dragQueryFile(hd, n, toCint 0, 0) + 1 (* Must add one for NULL *)
                    val buff = address (alloc buffsize Cchar)
                    val res = dragQueryFile(hd, n, buff, buffsize)
                in
                    fromCstring buff
                end
            in
                List.tabulate(nfiles, getFile)
            end
        end

        (* Call DragQueryPoint to find out where to drop the file(s). *)
        local
            val dragQueryPoint = call2 (shell "DragQueryPoint") (HDROP, POINTER) BOOL
            val (fromCpt, toCpt, ptSize) = breakConversion POINT
        in
            fun DragQueryPoint (hd: HDROP): POINT * bool =
            let
                val v = alloc 1 ptSize
                val res = dragQueryPoint(hd, address v)
            in
                (fromCpt v, res)
            end
        end
    end
end;
