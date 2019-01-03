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
            val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
        in
            fun ApplicationInstance() = winCall (1103, ())
        end

        local
            val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
        in
            fun MainWindow() = winCall (1104, ())
        end

    end
end;
