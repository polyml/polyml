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

structure Keyboard:
  sig
    type HWND
    val EnableWindow : HWND * bool -> bool
    val GetActiveWindow : unit -> HWND option
    val GetFocus : unit -> HWND option
    val IsWindowEnabled : HWND -> bool
    val SetActiveWindow : HWND -> HWND
    val SetFocus : HWND option -> HWND option
  end =
struct
    local
        open Foreign Base
        fun checkWindow c = (checkResult(not(isHNull c)); c)
    in
        type HWND = HWND
        val EnableWindow = winCall2 (user "EnableWindow") (cHWND, cBool) cBool
        val GetActiveWindow = winCall0 (user "GetActiveWindow") () cHWNDOPT
        val GetFocus = winCall0 (user "GetFocus") () cHWNDOPT
        val IsWindowEnabled = winCall1 (user "IsWindowEnabled") (cHWND) cBool
        val SetActiveWindow =
            checkWindow o winCall1 (user "SetActiveWindow") (cHWND) cHWND

        (* The argument to SetFocus is an option because we may ignore input.
           The result may be null if there was an error or if no window had focus. *)
        val SetFocus = winCall1 (user "SetFocus") (cHWNDOPT) cHWNDOPT
    end
end;

(*
ActivateKeyboardLayout  
GetAsyncKeyState  
GetKeyboardLayout  
GetKeyboardLayoutList  
GetKeyboardLayoutName  
GetKeyboardState  
GetKeyNameText  
GetKeyState  
keybd_event  
LoadKeyboardLayout  
MapVirtualKey  
MapVirtualKeyEx  
OemKeyScan  
RegisterHotKey  
SendInput  
SetKeyboardState  
ToAscii  
ToAsciiEx  
ToUnicode  
ToUnicodeEx  
UnloadKeyboardLayout  
UnregisterHotKey  
VkKeyScan  
VkKeyScanEx  

Obsolete Functions

GetKBCodePage
*)
