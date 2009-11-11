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
structure Globals :
  sig
    type 'a HANDLE
    type HINSTANCE
    type HWND
    val hNull : 'a HANDLE
    val handleOfInt : int -> 'a HANDLE
    val intOfHandle : 'a HANDLE -> int
    val isHNull : 'a HANDLE -> bool

    val ApplicationInstance : unit -> HINSTANCE
    val GetLastError : unit -> OS.syserror
    val MainWindow : unit -> HWND
  end
 =
struct
local
    open CInterface
    open Base
in
    type 'a HANDLE = 'a HANDLE
    val hNull = hNull
    and isHNull = isHNull
    and handleOfInt = handleOfInt
    and intOfHandle = intOfHandle

    type HINSTANCE = HINSTANCE

    type HWND = HWND

    val GetLastError = Base.GetLastError

    local
        fun callWin n : int =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_os_specific (n, ())
    in
        fun ApplicationInstance(): HINSTANCE = handleOfInt(callWin 1103)
        and MainWindow(): HWND = handleOfInt(callWin 1104)
    end
end
end;
