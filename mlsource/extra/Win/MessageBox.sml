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
structure MessageBox :
sig
    type HWND

    val IDABORT : int
    val IDCANCEL : int
    val IDCLOSE : int
    val IDHELP : int
    val IDIGNORE : int
    val IDNO : int
    val IDOK : int
    val IDRETRY : int
    val IDYES : int

    structure MessageBoxStyle :
      sig
        include BIT_FLAGS
        val MB_ABORTRETRYIGNORE : flags
        val MB_APPLMODAL : flags
        val MB_DEFAULT_DESKTOP_ONLY : flags
        val MB_DEFBUTTON1 : flags
        val MB_DEFBUTTON2 : flags
        val MB_DEFBUTTON3 : flags
        val MB_DEFBUTTON4 : flags
        val MB_HELP : flags
        val MB_ICONASTERISK : flags
        val MB_ICONERROR : flags
        val MB_ICONEXCLAMATION : flags
        val MB_ICONHAND : flags
        val MB_ICONINFORMATION : flags
        val MB_ICONQUESTION : flags
        val MB_ICONSTOP : flags
        val MB_ICONWARNING : flags
        val MB_NOFOCUS : flags
        val MB_OK : flags
        val MB_OKCANCEL : flags
        val MB_RETRYCANCEL : flags
        val MB_RIGHT : flags
        val MB_RTLREADING : flags
        val MB_SERVICE_NOTIFICATION : flags
        val MB_SERVICE_NOTIFICATION_NT3X : flags
        val MB_SETFOREGROUND : flags
        val MB_SYSTEMMODAL : flags
        val MB_TASKMODAL : flags
        val MB_TOPMOST : flags
        val MB_USERICON : flags
        val MB_YESNO : flags
        val MB_YESNOCANCEL : flags
      end

    val MessageBox : HWND option * string * string * MessageBoxStyle.flags -> int
    val MessageBeep: MessageBoxStyle.flags -> unit

end
=
struct
    local
        open CInterface
        open Base
        open Globals
        fun user name = load_sym (load_lib "user32.dll") name
    in
        type HWND = HWND

        structure MessageBoxStyle =
        struct
            type flags = SysWord.word
            fun toWord f = f
            fun fromWord f = f
            val flags = List.foldl (fn (a, b) => SysWord.orb(a,b)) 0w0
            fun allSet (fl1, fl2) = SysWord.andb(fl1, fl2) = fl1
            fun anySet (fl1, fl2) = SysWord.andb(fl1, fl2) <> 0w0
            fun clear (fl1, fl2) = SysWord.andb(SysWord.notb fl1, fl2)
    
            val MB_OK                       = 0wx00000000
            val MB_OKCANCEL                 = 0wx00000001
            val MB_ABORTRETRYIGNORE         = 0wx00000002
            val MB_YESNOCANCEL              = 0wx00000003
            val MB_YESNO                    = 0wx00000004
            val MB_RETRYCANCEL              = 0wx00000005

            val MB_ICONHAND                 = 0wx00000010
            val MB_ICONQUESTION             = 0wx00000020
            val MB_ICONEXCLAMATION          = 0wx00000030
            val MB_ICONASTERISK             = 0wx00000040
            val MB_USERICON                 = 0wx00000080
            val MB_ICONWARNING              = MB_ICONEXCLAMATION
            val MB_ICONERROR                = MB_ICONHAND

            val MB_ICONINFORMATION          = MB_ICONASTERISK
            val MB_ICONSTOP                 = MB_ICONHAND

            val MB_DEFBUTTON1               = 0wx00000000
            val MB_DEFBUTTON2               = 0wx00000100
            val MB_DEFBUTTON3               = 0wx00000200
            val MB_DEFBUTTON4               = 0wx00000300

            val MB_APPLMODAL                = 0wx00000000
            val MB_SYSTEMMODAL              = 0wx00001000
            val MB_TASKMODAL                = 0wx00002000
            val MB_HELP                     = 0wx00004000 (* Help Button *)

            val MB_NOFOCUS                  = 0wx00008000
            val MB_SETFOREGROUND            = 0wx00010000
            val MB_DEFAULT_DESKTOP_ONLY     = 0wx00020000

            val MB_TOPMOST                  = 0wx00040000
            val MB_RIGHT                    = 0wx00080000
            val MB_RTLREADING               = 0wx00100000

            val MB_SERVICE_NOTIFICATION          = 0wx00200000
            val MB_SERVICE_NOTIFICATION_NT3X     = 0wx00040000
    
            val all = flags[MB_OK, MB_OKCANCEL, MB_ABORTRETRYIGNORE, MB_YESNOCANCEL,
                            MB_YESNO, MB_RETRYCANCEL, MB_ICONHAND, MB_ICONQUESTION, 
                            MB_ICONEXCLAMATION, MB_ICONASTERISK, MB_USERICON, MB_DEFBUTTON1,
                            MB_DEFBUTTON2, MB_DEFBUTTON3, MB_DEFBUTTON4, MB_APPLMODAL, 
                            MB_SYSTEMMODAL, MB_TASKMODAL, MB_HELP, MB_NOFOCUS, MB_SETFOREGROUND, 
                            MB_DEFAULT_DESKTOP_ONLY, MB_TOPMOST, MB_RIGHT, MB_RTLREADING,
                            MB_SERVICE_NOTIFICATION, MB_SERVICE_NOTIFICATION_NT3X]
    
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a,b)) all
        end

        (* Return values from a message box.  Should this be a datatype? *)
        val IDOK                = 1
        val IDCANCEL            = 2
        val IDABORT             = 3
        val IDRETRY             = 4
        val IDIGNORE            = 5
        val IDYES               = 6
        val IDNO                = 7
        val IDCLOSE             = 8
        val IDHELP              = 9

        val MBSTYLE: MessageBoxStyle.flags Conversion = WORD

        val MessageBox = call4 (user "MessageBoxA") (HWNDOPT, STRING, STRING, MBSTYLE) INT

        val MessageBeep = call1 (user "MessageBeep") MBSTYLE (SUCCESSSTATE "MessageBeep")

    end
end;
