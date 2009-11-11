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

(* Buttons. *)
structure Button:
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
        and BS_3STATE: flags and BS_AUTO3STATE: flags and BS_AUTOCHECKBOX: flags
        and BS_AUTORADIOBUTTON: flags and BS_BITMAP: flags and BS_BOTTOM: flags
        and BS_CENTER: flags and BS_CHECKBOX: flags and BS_DEFPUSHBUTTON: flags
        and BS_FLAT: flags and BS_GROUPBOX: flags and BS_ICON: flags and BS_LEFT: flags
        and BS_LEFTTEXT: flags and BS_MULTILINE: flags and BS_NOTIFY: flags
        and BS_OWNERDRAW: flags and BS_PUSHBUTTON: flags and BS_PUSHLIKE: flags
        and BS_RADIOBUTTON: flags and BS_RIGHT: flags and BS_RIGHTBUTTON: flags
        and BS_TEXT: flags and BS_TOP: flags and BS_USERBUTTON: flags and BS_VCENTER: flags
    end

    structure Notifications:
    sig
        val BN_CLICKED: int
        val BN_PAINT: int
        val BN_HILITE: int
        val BN_UNHILITE: int
        val BN_DISABLE: int
        val BN_DOUBLECLICKED: int
        val BN_PUSHED: int
        val BN_UNPUSHED: int
        val BN_DBLCLK: int
        val BN_SETFOCUS: int
        val BN_KILLFOCUS: int
    end

    structure State:
    sig
        val BST_UNCHECKED: int
        val BST_CHECKED: int
        val BST_INDETERMINATE: int
        val BST_PUSHED: int
        val BST_FOCUS: int
    end
    
end
=
struct
    structure Style =
    struct
        open Window.Style (* Include all the windows styles. *)
        type flags = Window.Style.flags (* Causes the type to print as Dialog.Style.flags. *)

        val BS_PUSHBUTTON: flags       = fromWord 0wx00000000
        val BS_DEFPUSHBUTTON: flags    = fromWord 0wx00000001
        val BS_CHECKBOX: flags         = fromWord 0wx00000002
        val BS_AUTOCHECKBOX: flags     = fromWord 0wx00000003
        val BS_RADIOBUTTON: flags      = fromWord 0wx00000004
        val BS_3STATE: flags           = fromWord 0wx00000005
        val BS_AUTO3STATE: flags       = fromWord 0wx00000006
        val BS_GROUPBOX: flags         = fromWord 0wx00000007
        val BS_USERBUTTON: flags       = fromWord 0wx00000008
        val BS_AUTORADIOBUTTON: flags  = fromWord 0wx00000009
        val BS_OWNERDRAW: flags        = fromWord 0wx0000000B
        val BS_LEFTTEXT: flags         = fromWord 0wx00000020
        val BS_TEXT: flags             = fromWord 0wx00000000
        val BS_ICON: flags             = fromWord 0wx00000040
        val BS_BITMAP: flags           = fromWord 0wx00000080
        val BS_LEFT: flags             = fromWord 0wx00000100
        val BS_RIGHT: flags            = fromWord 0wx00000200
        val BS_CENTER: flags           = fromWord 0wx00000300
        val BS_TOP: flags              = fromWord 0wx00000400
        val BS_BOTTOM: flags           = fromWord 0wx00000800
        val BS_VCENTER: flags          = fromWord 0wx00000C00
        val BS_PUSHLIKE: flags         = fromWord 0wx00001000
        val BS_MULTILINE: flags        = fromWord 0wx00002000
        val BS_NOTIFY: flags           = fromWord 0wx00004000
        val BS_FLAT: flags             = fromWord 0wx00008000
        val BS_RIGHTBUTTON: flags      = BS_LEFTTEXT

        val all = flags[Window.Style.all, BS_PUSHBUTTON, BS_DEFPUSHBUTTON, BS_CHECKBOX,
                        BS_AUTOCHECKBOX, BS_RADIOBUTTON, BS_3STATE, BS_AUTO3STATE, BS_GROUPBOX,
                        BS_USERBUTTON, BS_AUTORADIOBUTTON, BS_OWNERDRAW, BS_LEFTTEXT, BS_TEXT,
                        BS_ICON, BS_BITMAP, BS_LEFT, BS_RIGHT, BS_CENTER, BS_TOP, BS_BOTTOM,
                        BS_VCENTER, BS_PUSHLIKE, BS_MULTILINE, BS_NOTIFY, BS_FLAT]

        val intersect =
            List.foldl (fn (a, b) => fromWord(SysWord.andb(toWord a, toWord b))) all
    end

    structure Notifications =
    struct
        val BN_CLICKED          = 0
        val BN_PAINT            = 1
        val BN_HILITE           = 2
        val BN_UNHILITE         = 3
        val BN_DISABLE          = 4
        val BN_DOUBLECLICKED    = 5
        val BN_PUSHED           = BN_HILITE
        val BN_UNPUSHED         = BN_UNHILITE
        val BN_DBLCLK           = BN_DOUBLECLICKED
        val BN_SETFOCUS         = 6
        val BN_KILLFOCUS        = 7
    end

    (* These are returned by SendMessage(button, BM_GETCHECK) so need to be integers. *)
    structure State =
    struct
        val BST_UNCHECKED      = 0x0000
        val BST_CHECKED        = 0x0001
        val BST_INDETERMINATE  = 0x0002
        val BST_PUSHED         = 0x0004
        val BST_FOCUS          = 0x0008
    end

end;

(*
let
    open Button.Style

    fun getType w =
    let
        val typeField = fromWord(SysWord.andb(toWord w, 0wx0f))
    in
        if typeField = BS_PUSHBUTTON then "BS_PUSHBUTTON"
        else if typeField = BS_DEFPUSHBUTTON then "BS_DEFPUSHBUTTON"
        else if typeField = BS_CHECKBOX then "BS_CHECKBOX"
        else if typeField = BS_AUTOCHECKBOX then "BS_AUTOCHECKBOX"
        else if typeField = BS_RADIOBUTTON then "BS_RADIOBUTTON"
        else if typeField = BS_3STATE then "BS_3STATE"
        else if typeField = BS_AUTO3STATE then "BS_AUTO3STATE"
        else if typeField = BS_GROUPBOX then "BS_GROUPBOX"
        else if typeField = BS_USERBUTTON then "BS_USERBUTTON"
        else if typeField = BS_AUTORADIOBUTTON then "BS_AUTORADIOBUTTON"
        else if typeField = BS_OWNERDRAW then "BS_OWNERDRAW"
        else "??"
    end

    val flagTable =
        [(BS_LEFTTEXT,          "BS_LEFTTEXT"),
         (BS_ICON,              "BS_ICON"),
         (BS_BITMAP,            "BS_BITMAP"),
         (BS_CENTER,            "BS_CENTER"), (* Must come before the next two. *)
         (BS_LEFT,              "BS_LEFT"),
         (BS_RIGHT,             "BS_RIGHT"),
         (BS_VCENTER,           "BS_VCENTER"), (* Must come before the next two. *)
         (BS_TOP,               "BS_TOP"),
         (BS_BOTTOM,            "BS_BOTTOM"),
         (BS_PUSHLIKE,          "BS_PUSHLIKE"),
         (BS_MULTILINE,         "BS_MULTILINE"),
         (BS_NOTIFY,            "BS_NOTIFY"),
         (BS_FLAT,              "BS_FLAT"),
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