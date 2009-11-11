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

(* Static windows e.g. labels. *)
structure Static:
sig
    structure Style:
        sig
        include BIT_FLAGS where type flags = Window.Style.flags
        val WS_OVERLAPPED: flags and WS_POPUP: flags and WS_CHILD: flags and WS_MINIMIZE: flags
        and WS_VISIBLE: flags and WS_DISABLED:flags and WS_CLIPSIBLINGS:flags
        and WS_CLIPCHILDREN:flags and WS_MAXIMIZE:flags and WS_CAPTION:flags
        and WS_BORDER:flags and WS_DLGFRAME:flags and WS_VSCROLL:flags and WS_HSCROLL:flags
        and WS_SYSMENU:flags and WS_THICKFRAME:flags and WS_GROUP:flags and WS_TABSTOP:flags
        and WS_MINIMIZEBOX:flags and WS_MAXIMIZEBOX:flags and WS_TILED:flags and WS_ICONIC:flags
        and WS_SIZEBOX:flags and WS_OVERLAPPEDWINDOW:flags and WS_TILEDWINDOW:flags
        and WS_POPUPWINDOW:flags and WS_CHILDWINDOW:flags
        and SS_LEFT: flags and SS_CENTER: flags and SS_RIGHT: flags and SS_ICON: flags
        and SS_BLACKRECT: flags and SS_GRAYRECT: flags and SS_WHITERECT: flags
        and SS_BLACKFRAME: flags and SS_GRAYFRAME: flags and SS_WHITEFRAME: flags
        and SS_USERITEM: flags and SS_SIMPLE: flags and SS_LEFTNOWORDWRAP: flags
        and SS_OWNERDRAW: flags and SS_BITMAP: flags and SS_ENHMETAFILE: flags
        and SS_ETCHEDHORZ: flags and SS_ETCHEDVERT: flags and SS_ETCHEDFRAME: flags
        and SS_TYPEMASK: flags and SS_NOPREFIX: flags and SS_NOTIFY: flags and SS_CENTERIMAGE: flags
        and SS_RIGHTJUST: flags and SS_REALSIZEIMAGE: flags and SS_SUNKEN: flags
        and SS_ENDELLIPSIS: flags and SS_PATHELLIPSIS: flags and SS_WORDELLIPSIS: flags
        and SS_ELLIPSISMASK: flags
        end

    structure Notifications:
    sig
        val STN_CLICKED: int
        val STN_DBLCLK: int
        val STN_ENABLE: int
        val STN_DISABLE: int
    end
end
=
struct
    structure Style =
    struct
        open Window.Style (* Include all the windows styles. *)

        val SS_LEFT: flags             = fromWord 0wx00000000
        val SS_CENTER: flags           = fromWord 0wx00000001
        val SS_RIGHT: flags            = fromWord 0wx00000002
        val SS_ICON: flags             = fromWord 0wx00000003
        val SS_BLACKRECT: flags        = fromWord 0wx00000004
        val SS_GRAYRECT: flags         = fromWord 0wx00000005
        val SS_WHITERECT: flags        = fromWord 0wx00000006
        val SS_BLACKFRAME: flags       = fromWord 0wx00000007
        val SS_GRAYFRAME: flags        = fromWord 0wx00000008
        val SS_WHITEFRAME: flags       = fromWord 0wx00000009
        val SS_USERITEM: flags         = fromWord 0wx0000000A
        val SS_SIMPLE: flags           = fromWord 0wx0000000B
        val SS_LEFTNOWORDWRAP: flags   = fromWord 0wx0000000C
        val SS_OWNERDRAW: flags        = fromWord 0wx0000000D
        val SS_BITMAP: flags           = fromWord 0wx0000000E
        val SS_ENHMETAFILE: flags      = fromWord 0wx0000000F
        val SS_ETCHEDHORZ: flags       = fromWord 0wx00000010
        val SS_ETCHEDVERT: flags       = fromWord 0wx00000011
        val SS_ETCHEDFRAME: flags      = fromWord 0wx00000012
        val SS_TYPEMASK: flags         = fromWord 0wx0000001F
        val SS_NOPREFIX: flags         = fromWord 0wx00000080
        val SS_NOTIFY: flags           = fromWord 0wx00000100
        val SS_CENTERIMAGE: flags      = fromWord 0wx00000200
        val SS_RIGHTJUST: flags        = fromWord 0wx00000400
        val SS_REALSIZEIMAGE: flags    = fromWord 0wx00000800
        val SS_SUNKEN: flags           = fromWord 0wx00001000
        val SS_ENDELLIPSIS: flags      = fromWord 0wx00004000
        val SS_PATHELLIPSIS: flags     = fromWord 0wx00008000
        val SS_WORDELLIPSIS: flags     = fromWord 0wx0000C000
        val SS_ELLIPSISMASK: flags     = fromWord 0wx0000C000

        val all = flags[Window.Style.all, SS_LEFT, SS_CENTER, SS_RIGHT, SS_ICON, SS_BLACKRECT,
                        SS_GRAYRECT, SS_WHITERECT, SS_BLACKFRAME, SS_GRAYFRAME,
                        SS_WHITEFRAME, SS_USERITEM, SS_SIMPLE, SS_LEFTNOWORDWRAP,
                        SS_OWNERDRAW, SS_BITMAP, SS_ENHMETAFILE, SS_ETCHEDHORZ,
                        SS_ETCHEDVERT, SS_ETCHEDFRAME, SS_TYPEMASK, SS_NOPREFIX,
                        SS_NOTIFY, SS_CENTERIMAGE, SS_RIGHTJUST, SS_REALSIZEIMAGE,
                        SS_SUNKEN, SS_ENDELLIPSIS, SS_PATHELLIPSIS, SS_WORDELLIPSIS,
                        SS_ELLIPSISMASK]
        val intersect =
            List.foldl (fn (a, b) => fromWord(SysWord.andb(toWord a, toWord b))) all
    end

    structure Notifications =
    struct
        val STN_CLICKED         = 0
        val STN_DBLCLK          = 1
        val STN_ENABLE          = 2
        val STN_DISABLE         = 3
    end
end;

(*
let
    open Static.Style

    fun getType w =
    let
        val typeField = fromWord(SysWord.andb(toWord w, toWord SS_TYPEMASK))
    in
        if typeField = SS_LEFT then "SS_LEFT"
        else if typeField = SS_CENTER then "SS_CENTER"
        else if typeField = SS_RIGHT then "SS_RIGHT"
        else if typeField = SS_ICON then "SS_ICON"
        else if typeField = SS_BLACKRECT then "SS_BLACKRECT"
        else if typeField = SS_GRAYRECT then "SS_GRAYRECT"
        else if typeField = SS_WHITERECT then "SS_WHITERECT"
        else if typeField = SS_BLACKFRAME then "SS_BLACKFRAME"
        else if typeField = SS_GRAYFRAME then "SS_GRAYFRAME"
        else if typeField = SS_WHITEFRAME then "SS_WHITEFRAME"
        else if typeField = SS_USERITEM then "SS_USERITEM"
        else if typeField = SS_SIMPLE then "SS_SIMPLE"
        else if typeField = SS_LEFTNOWORDWRAP then "SS_LEFTNOWORDWRAP"
        else if typeField = SS_OWNERDRAW then "SS_OWNERDRAW"
        else if typeField = SS_BITMAP then "SS_BITMAP"
        else if typeField = SS_ENHMETAFILE then "SS_ENHMETAFILE"
        else if typeField = SS_ETCHEDHORZ then "SS_ETCHEDHORZ"
        else if typeField = SS_ETCHEDVERT then "SS_ETCHEDVERT"
        else if typeField = SS_ETCHEDFRAME then "SS_ETCHEDFRAME"
        else "??"
    end

    val flagTable =
        [(SS_NOPREFIX,          "SS_NOPREFIX"),
         (SS_NOTIFY,            "SS_NOTIFY"),
         (SS_CENTERIMAGE,       "SS_CENTERIMAGE"),
         (SS_RIGHTJUST,         "SS_RIGHTJUST"),
         (SS_REALSIZEIMAGE,     "SS_REALSIZEIMAGE"),
         (SS_SUNKEN,            "SS_SUNKEN"),
         (SS_WORDELLIPSIS,      "SS_WORDELLIPSIS"), (* Must come before the next two. *)
         (SS_ENDELLIPSIS,       "SS_ENDELLIPSIS"),
         (SS_PATHELLIPSIS,      "SS_PATHELLIPSIS"),
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
        
          val stringFlags = getType x :: accumulateFlags x flagTable
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