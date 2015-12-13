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
        open Foreign Base
    in
        type HDROP = HDROP and HWND = HWND
        type POINT = POINT

        (* Call DragAcceptFiles to accept files. *)
        val DragAcceptFiles = winCall2 (shell "DragAcceptFiles") (cHWND,cBool) cVoid

        (* Call DragFinish when finished processing a WM_DROP message. *)
        and DragFinish = winCall1 (shell "DragFinish") (cHDROP) cVoid

        (* Call DragQueryFile to get the file(s). *)
        local
            val dragQueryFile = winCall4 (shell "DragQueryFileA") (cHDROP,cUint,cPointer,cUint) cUint
        in
            fun DragQueryFile (hd: HDROP): string list =
            let
                val nfiles = dragQueryFile(hd, ~1, Memory.null, 0)
                fun getFile n =
                let
                    val buffsize =
                        dragQueryFile(hd, n, Memory.null, 0) + 1 (* Must add one for NULL *)
                    open Memory
                    val buff = malloc(Word.fromInt buffsize)
                    val _ =
                        dragQueryFile(hd, n, buff, buffsize)
                            handle ex => (free buff; raise ex)
                in
                    fromCstring buff before free buff
                end
            in
                List.tabulate(nfiles, getFile)
            end
        end

        (* Call DragQueryPoint to find out where to drop the file(s). *)
        local
            val dragQueryPoint = winCall2 (shell "DragQueryPoint") (cHDROP, cStar cPoint) cBool
        in
            fun DragQueryPoint (hd: HDROP): POINT * bool =
            let
                val r = ref {x=0, y=0}
                val res = dragQueryPoint(hd, r)
            in
                (!r, res)
            end
        end
    end
end;
