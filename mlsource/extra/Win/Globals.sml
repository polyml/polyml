(*
    Copyright (c) 2001, 2015, 2019
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
structure Globals :
  sig
    eqtype 'a HANDLE
    eqtype HINSTANCE
    eqtype HWND
    val hNull : 'a HANDLE
    val isHNull : 'a HANDLE -> bool

    val ApplicationInstance : unit -> HINSTANCE
    val GetLastError : unit -> OS.syserror
    val MainWindow : unit -> HWND
  end
 =
struct
    local
        open Foreign
        open Base
    in
        type 'a HANDLE = 'a HANDLE
        val hNull = hNull
        and isHNull = isHNull
        type HINSTANCE = HINSTANCE

        type HWND = HWND

        val GetLastError = Base.GetLastError

        local
            val getModHandle =
                winCall1 (kernel "GetModuleHandleA") (cOptionPtr cString) cHINSTANCE
            (* The current hInstance is also returned as Foreign.System.loadExecutable. *)
        in
            fun ApplicationInstance() = getModHandle NONE
        end

        local
            val FindWindow =
                winCall2 (user "FindWindowA") (STRINGOPT, STRINGOPT) cHWND
        in
            fun MainWindow() = FindWindow(SOME "PolyMLWindowClass", SOME "Poly/ML")
        end

    end
end;
