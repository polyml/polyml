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

(* Edit windows. *)
structure Edit:
sig
    structure Style:
    sig
        (* We use the same type so we can use this everywhere we can use
           the general window style. *)
        include BIT_FLAGS where type flags = Window.Style.flags
        val WS_OVERLAPPED: flags and WS_POPUP: flags and WS_CHILD: flags and WS_MINIMIZE: flags
        and WS_VISIBLE: flags and WS_DISABLED:flags and WS_CLIPSIBLINGS:flags
        and WS_CLIPCHILDREN:flags and WS_MAXIMIZE:flags and WS_CAPTION:flags
        and WS_BORDER:flags and WS_DLGFRAME:flags and WS_VSCROLL:flags and WS_HSCROLL:flags
        and WS_SYSMENU:flags and WS_THICKFRAME:flags and WS_GROUP:flags and WS_TABSTOP:flags
        and WS_MINIMIZEBOX:flags and WS_MAXIMIZEBOX:flags and WS_TILED:flags and WS_ICONIC:flags
        and WS_SIZEBOX:flags and WS_OVERLAPPEDWINDOW:flags and WS_TILEDWINDOW:flags
        and WS_POPUPWINDOW:flags and WS_CHILDWINDOW:flags
        and ES_LEFT: flags and ES_CENTER:flags and ES_RIGHT:flags and ES_MULTILINE:flags
        and ES_UPPERCASE: flags and ES_LOWERCASE: flags and ES_PASSWORD: flags
        and ES_AUTOVSCROLL: flags and ES_AUTOHSCROLL: flags and ES_NOHIDESEL: flags
        and ES_OEMCONVERT: flags and ES_READONLY: flags and ES_WANTRETURN: flags and ES_NUMBER: flags
    end

    structure Notifications:
    sig
        val EN_SETFOCUS: int
        val EN_KILLFOCUS: int
        val EN_CHANGE: int
        val EN_UPDATE: int
        val EN_ERRSPACE: int
        val EN_MAXTEXT: int
        val EN_HSCROLL: int
        val EN_VSCROLL: int
    end
end
=
struct
    structure Style =
    struct
        open Window.Style (* Include all the windows styles. *)

        val ES_LEFT: flags             = fromWord 0wx0000
        val ES_CENTER: flags           = fromWord 0wx0001
        val ES_RIGHT: flags            = fromWord 0wx0002
        val ES_MULTILINE: flags        = fromWord 0wx0004
        val ES_UPPERCASE: flags        = fromWord 0wx0008
        val ES_LOWERCASE: flags        = fromWord 0wx0010
        val ES_PASSWORD: flags         = fromWord 0wx0020
        val ES_AUTOVSCROLL: flags      = fromWord 0wx0040
        val ES_AUTOHSCROLL: flags      = fromWord 0wx0080
        val ES_NOHIDESEL: flags        = fromWord 0wx0100
        val ES_OEMCONVERT: flags       = fromWord 0wx0400
        val ES_READONLY: flags         = fromWord 0wx0800
        val ES_WANTRETURN: flags       = fromWord 0wx1000
        val ES_NUMBER: flags           = fromWord 0wx2000

        val all = flags[Window.Style.all, ES_LEFT, ES_CENTER, ES_RIGHT, ES_MULTILINE,
                        ES_UPPERCASE, ES_LOWERCASE, ES_PASSWORD, ES_AUTOVSCROLL,
                        ES_AUTOHSCROLL, ES_NOHIDESEL, ES_OEMCONVERT, ES_READONLY,
                        ES_WANTRETURN, ES_NUMBER]

        val intersect =
            List.foldl (fn (a, b) => fromWord(SysWord.andb(toWord a, toWord b))) all
    end

    structure Notifications =
    struct
        val EN_SETFOCUS         = 0x0100
        val EN_KILLFOCUS        = 0x0200
        val EN_CHANGE           = 0x0300
        val EN_UPDATE           = 0x0400
        val EN_ERRSPACE         = 0x0500
        val EN_MAXTEXT          = 0x0501
        val EN_HSCROLL          = 0x0601
        val EN_VSCROLL          = 0x0602
    end
end;

(*
let
    open Edit.Style

    val flagTable =
        [(ES_CENTER,            "ES_CENTER"),
         (ES_RIGHT,             "ES_RIGHT"),
         (ES_MULTILINE,         "ES_MULTILINE"),
         (ES_UPPERCASE,         "ES_UPPERCASE"),
         (ES_LOWERCASE,         "ES_LOWERCASE"),
         (ES_PASSWORD,          "ES_PASSWORD"),
         (ES_AUTOVSCROLL,       "ES_AUTOVSCROLL"),
         (ES_AUTOHSCROLL,       "ES_AUTOHSCROLL"),
         (ES_NOHIDESEL,         "ES_NOHIDESEL"),
         (ES_OEMCONVERT,        "ES_OEMCONVERT"),
         (ES_READONLY,          "ES_READONLY"),
         (ES_WANTRETURN,        "ES_WANTRETURN"),
         (ES_NUMBER,            "ES_NUMBER"),
         (WS_POPUP,             "WS_POPUP"),
         (WS_CHILD,             "WS_CHILD"),
         (WS_MINIMIZE,          "WS_MINIMIZE"),
         (WS_VISIBLE,           "WS_VISIBLE"),
         (WS_DISABLED,          "WS_DISABLED"),
         (WS_CLIPSIBLINGS,      "WS_CLIPSIBLINGS"),
         (WS_CLIPCHILDREN,      "WS_CLIPCHILDREN"),
         (WS_MAXIMIZE,          "WS_MAXIMIZE"),
         (WS_CAPTION,           "WS_CAPTION"),
         (WS_BORDER,            "WS_BORDER"),
         (WS_DLGFRAME,          "WS_DLGFRAME"),
         (WS_VSCROLL,           "WS_VSCROLL"),
         (WS_HSCROLL,           "WS_HSCROLL"),
         (WS_SYSMENU,           "WS_SYSMENU"),
         (WS_THICKFRAME,        "WS_THICKFRAME"),
         (WS_GROUP,             "WS_GROUP"),
         (WS_TABSTOP,           "WS_TABSTOP"),
         (WS_MINIMIZEBOX,       "WS_MINIMIZEBOX"),
         (WS_MAXIMIZEBOX,       "WS_MAXIMIZEBOX")]

    fun accumulateFlags f [] = []
     |  accumulateFlags f ((w, s)::t) =
        if allSet(w, f) then s :: accumulateFlags(clear(w, f)) t
        else accumulateFlags f t

    fun printFlags(put, beg, brk, nd) depth _ x =
        (* This is just the code to print a list. *)
        let
        
          val stringFlags = accumulateFlags x flagTable
          fun plist [] depth = ()
           |  plist _ 0 = put "..."
           |  plist [h]    depth = put h 
           |  plist (h::t) depth =
                  ( put (h^",");
                    brk (1, 0);
                    plist t (depth - 1)
                  )
        in
          beg (3, false);
          put "[";
          if depth <= 0 then put "..." else plist stringFlags depth;
          put "]";
          nd ()
        end
in
    PolyML.install_pp printFlags
end;
*)