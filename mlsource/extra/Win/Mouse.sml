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
        open Foreign Base
    in
        type HWND = HWND and POINT = POINT

        val GetCapture = winCall0 (user "GetCapture") () cHWNDOPT
        val SetCapture = winCall1 (user "SetCapture") (cHWND) cHWNDOPT
        val ReleaseCapture = winCall0 (user "ReleaseCapture") () (successState "ReleaseCapture")
        val SetDoubleClickTime =
            winCall1 (user "SetDoubleClickTime") (cUint) (successState "SetDoubleClickTime") o
                Time.toMilliseconds
        val GetDoubleClickTime =
            Time.fromMilliseconds o winCall0 (user "GetDoubleClickTime") () cUint 
        val SwapMouseButton = winCall1 (user "SwapMouseButton") (cBool) cBool
        val DragDetect = winCall2 (user "DragDetect") (cHWND, cPoint) cBool
    end
end;

(*
GetMouseMovePoints  - NT 5.0 and Windows 98 only
mouse_event  
TrackMouseEvent  
*)
