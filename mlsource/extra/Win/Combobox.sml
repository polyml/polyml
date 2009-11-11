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

(* Comboboxes. *)
structure Combobox:
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
        and CBS_SIMPLE: flags and CBS_DROPDOWN: flags and CBS_DROPDOWNLIST: flags
        and CBS_OWNERDRAWFIXED: flags and CBS_OWNERDRAWVARIABLE: flags and CBS_AUTOHSCROLL: flags
        and CBS_OEMCONVERT: flags and CBS_SORT: flags and CBS_HASSTRINGS: flags
        and CBS_NOINTEGRALHEIGHT: flags and CBS_DISABLENOSCROLL: flags
        and CBS_UPPERCASE: flags and CBS_LOWERCASE: flags
    end

    structure Notifications:
    sig
        val CBN_SELCHANGE: int
        val CBN_DBLCLK: int
        val CBN_SETFOCUS: int
        val CBN_KILLFOCUS: int
        val CBN_EDITCHANGE: int
        val CBN_EDITUPDATE: int
        val CBN_DROPDOWN: int
        val CBN_CLOSEUP: int
        val CBN_SELENDOK: int
        val CBN_SELENDCANCEL: int
    end

    datatype CBDirAttr =
        DDL_READWRITE | DDL_READONLY | DDL_HIDDEN | DDL_SYSTEM | DDL_DIRECTORY |
        DDL_ARCHIVE | DDL_POSTMSGS | DDL_DRIVES | DDL_EXCLUSIVE
end
=
struct
    open ComboBase

    structure Style =
    struct
        open Window.Style (* Include all the windows styles. *)

        val CBS_SIMPLE            = fromWord 0wx0001
        val CBS_DROPDOWN          = fromWord 0wx0002
        val CBS_DROPDOWNLIST      = fromWord 0wx0003
        val CBS_OWNERDRAWFIXED    = fromWord 0wx0010
        val CBS_OWNERDRAWVARIABLE = fromWord 0wx0020
        val CBS_AUTOHSCROLL       = fromWord 0wx0040
        val CBS_OEMCONVERT        = fromWord 0wx0080
        val CBS_SORT              = fromWord 0wx0100
        val CBS_HASSTRINGS        = fromWord 0wx0200
        val CBS_NOINTEGRALHEIGHT  = fromWord 0wx0400
        val CBS_DISABLENOSCROLL   = fromWord 0wx0800
        val CBS_UPPERCASE         = fromWord 0wx2000
        val CBS_LOWERCASE         = fromWord 0wx4000

        val all = flags[Window.Style.all, CBS_SIMPLE, CBS_DROPDOWN, CBS_DROPDOWNLIST,
                        CBS_OWNERDRAWFIXED, CBS_OWNERDRAWVARIABLE, CBS_AUTOHSCROLL,
                        CBS_OEMCONVERT, CBS_SORT, CBS_HASSTRINGS, CBS_NOINTEGRALHEIGHT,
                        CBS_DISABLENOSCROLL, CBS_UPPERCASE, CBS_LOWERCASE]

        val intersect =
            List.foldl (fn (a, b) => fromWord(SysWord.andb(toWord a, toWord b))) all
    end

    structure Notifications =
    struct
        val CBN_SELCHANGE       = 1
        val CBN_DBLCLK          = 2
        val CBN_SETFOCUS        = 3
        val CBN_KILLFOCUS       = 4
        val CBN_EDITCHANGE      = 5
        val CBN_EDITUPDATE      = 6
        val CBN_DROPDOWN        = 7
        val CBN_CLOSEUP         = 8
        val CBN_SELENDOK        = 9
        val CBN_SELENDCANCEL    = 10
    end
(*
DlgDirListComboBox  
DlgDirSelectEx  
DlgDirSelectComboBoxEx   
*)
end;

(*
let
    open Combobox.Style

    val flagTable =
        [(CBS_DROPDOWNLIST,     "CBS_DROPDOWNLIST"), (* Must come before the next two. *)
         (CBS_SIMPLE,           "CBS_SIMPLE"),
         (CBS_DROPDOWN,         "CBS_DROPDOWN"),
         (CBS_OWNERDRAWFIXED,   "CBS_OWNERDRAWFIXED"),
         (CBS_OWNERDRAWVARIABLE, "CBS_OWNERDRAWVARIABLE"),
         (CBS_AUTOHSCROLL,      "CBS_AUTOHSCROLL"),
         (CBS_OEMCONVERT,       "CBS_OEMCONVERT"),
         (CBS_SORT,             "CBS_SORT"),
         (CBS_HASSTRINGS,       "CBS_HASSTRINGS"),
         (CBS_NOINTEGRALHEIGHT, "CBS_NOINTEGRALHEIGHT"),
         (CBS_DISABLENOSCROLL,  "CBS_DISABLENOSCROLL"),
         (CBS_UPPERCASE,        "CBS_UPPERCASE"),
         (CBS_LOWERCASE,        "CBS_LOWERCASE"),
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
