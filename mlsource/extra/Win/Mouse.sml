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

structure Mouse :
  sig
    type HWND
    type POINT = { x: int, y: int }
    val DragDetect : HWND * POINT -> bool
    val GetCapture : unit -> HWND option
    val GetDoubleClickTime : unit -> Time.time
    val ReleaseCapture : unit -> unit
    val SetCapture : HWND -> HWND option
    val SetDoubleClickTime : Time.time -> unit
    val SwapMouseButton : bool -> bool
  end =
struct
    local
        open CInterface Base
    in
        type HWND = HWND and POINT = POINT

        val GetCapture = call0 (user "GetCapture") () HWNDOPT
        val SetCapture = call1 (user "SetCapture") (HWND) HWNDOPT
        val ReleaseCapture = call0 (user "ReleaseCapture") () (SUCCESSSTATE "ReleaseCapture")
        val SetDoubleClickTime =
            call1 (user "SetDoubleClickTime") (INT) (SUCCESSSTATE "SetDoubleClickTime") o
                Time.toMilliseconds
        val GetDoubleClickTime =
            Time.fromMilliseconds o call0 (user "GetDoubleClickTime") () INT 
        val SwapMouseButton = call1 (user "SwapMouseButton") (BOOL) BOOL
        val DragDetect = call2 (user "DragDetect") (HWND, POINT) BOOL
    end
end;

(*
GetMouseMovePoints  - NT 5.0 and Windows 98 only
mouse_event  
TrackMouseEvent  
*)
