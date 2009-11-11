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

(* Listboxes. *)
structure Listbox:
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
        and WS_POPUPWINDOW: flags and WS_CHILDWINDOW: flags
        and LBS_NOTIFY: flags and LBS_SORT: flags and LBS_NOREDRAW: flags and LBS_MULTIPLESEL: flags
        and LBS_OWNERDRAWFIXED: flags and LBS_OWNERDRAWVARIABLE: flags and LBS_HASSTRINGS: flags
        and LBS_USETABSTOPS: flags and LBS_NOINTEGRALHEIGHT: flags and LBS_MULTICOLUMN: flags
        and LBS_WANTKEYBOARDINPUT: flags and LBS_EXTENDEDSEL: flags and LBS_DISABLENOSCROLL: flags
        and LBS_NODATA: flags and LBS_NOSEL: flags and LBS_STANDARD: flags
    end

    structure Notifications:
    sig
        val LBN_SELCHANGE: int
        val LBN_DBLCLK: int
        val LBN_SELCANCEL: int
        val LBN_SETFOCUS: int
        val LBN_KILLFOCUS: int
    end

    datatype LBDirAttr =
        DDL_READWRITE | DDL_READONLY | DDL_HIDDEN | DDL_SYSTEM | DDL_DIRECTORY |
        DDL_ARCHIVE | DDL_POSTMSGS | DDL_DRIVES | DDL_EXCLUSIVE
end
=
struct
    structure Style =
    struct
        open Window.Style (* Include all the windows styles. *)

        val LBS_NOTIFY            = fromWord 0wx0001
        val LBS_SORT              = fromWord 0wx0002
        val LBS_NOREDRAW          = fromWord 0wx0004
        val LBS_MULTIPLESEL       = fromWord 0wx0008
        val LBS_OWNERDRAWFIXED    = fromWord 0wx0010
        val LBS_OWNERDRAWVARIABLE = fromWord 0wx0020
        val LBS_HASSTRINGS        = fromWord 0wx0040
        val LBS_USETABSTOPS       = fromWord 0wx0080
        val LBS_NOINTEGRALHEIGHT  = fromWord 0wx0100
        val LBS_MULTICOLUMN       = fromWord 0wx0200
        val LBS_WANTKEYBOARDINPUT = fromWord 0wx0400
        val LBS_EXTENDEDSEL       = fromWord 0wx0800
        val LBS_DISABLENOSCROLL   = fromWord 0wx1000
        val LBS_NODATA            = fromWord 0wx2000
        val LBS_NOSEL             = fromWord 0wx4000
        val LBS_STANDARD          = flags[LBS_NOTIFY, LBS_SORT, WS_VSCROLL, WS_BORDER]

        val all = flags[Window.Style.all, LBS_NOTIFY, LBS_SORT, LBS_NOREDRAW, LBS_MULTIPLESEL,
                        LBS_OWNERDRAWFIXED, LBS_OWNERDRAWVARIABLE, LBS_HASSTRINGS,
                        LBS_USETABSTOPS, LBS_NOINTEGRALHEIGHT, LBS_MULTICOLUMN,
                        LBS_WANTKEYBOARDINPUT, LBS_EXTENDEDSEL, LBS_DISABLENOSCROLL,
                        LBS_NODATA, LBS_NOSEL]

        val intersect =
            List.foldl (fn (a, b) => fromWord(SysWord.andb(toWord a, toWord b))) all
    end

    structure Notifications =
    struct
        val LBN_SELCHANGE       = 1
        val LBN_DBLCLK          = 2
        val LBN_SELCANCEL       = 3
        val LBN_SETFOCUS        = 4
        val LBN_KILLFOCUS       = 5
    end

    datatype LBDirAttr = datatype ComboBase.CBDirAttr
end;

(*
let
    open Listbox.Style

    val flagTable =
        [(LBS_NOTIFY,           "LBS_NOTIFY"),
         (LBS_SORT,             "LBS_SORT"),
         (LBS_NOREDRAW,         "LBS_NOREDRAW"),
         (LBS_MULTIPLESEL,      "LBS_MULTIPLESEL"),
         (LBS_OWNERDRAWFIXED,   "LBS_OWNERDRAWFIXED"),
         (LBS_OWNERDRAWVARIABLE, "LBS_OWNERDRAWVARIABLE"),
         (LBS_HASSTRINGS,       "LBS_HASSTRINGS"),
         (LBS_USETABSTOPS,      "LBS_USETABSTOPS"),
         (LBS_NOINTEGRALHEIGHT, "LBS_NOINTEGRALHEIGHT"),
         (LBS_MULTICOLUMN,      "LBS_MULTICOLUMN"),
         (LBS_WANTKEYBOARDINPUT, "LBS_WANTKEYBOARDINPUT"),
         (LBS_EXTENDEDSEL,      "LBS_EXTENDEDSEL"),
         (LBS_DISABLENOSCROLL,  "LBS_DISABLENOSCROLL"),
         (LBS_NODATA,           "LBS_NODATA"),
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