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

(* Scrollbars. *)
structure Scrollbar:
sig
    type HWND and HDC and HRGN
    type RECT =  { left: int, top: int, right: int, bottom: int }

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
        and SBS_HORZ:flags and SBS_VERT:flags and SBS_TOPALIGN:flags and SBS_LEFTALIGN:flags
        and SBS_BOTTOMALIGN:flags and SBS_RIGHTALIGN:flags and SBS_SIZEBOXTOPLEFTALIGN:flags
        and SBS_SIZEBOXBOTTOMRIGHTALIGN:flags and SBS_SIZEBOX:flags and SBS_SIZEGRIP:flags
    end

    type enableArrows = { enableLeftUp: bool, enableRightDown: bool }

    val ESB_ENABLE_BOTH: enableArrows
    val ESB_DISABLE_BOTH: enableArrows
    val ESB_DISABLE_LEFT: enableArrows
    val ESB_DISABLE_RIGHT: enableArrows
    val ESB_DISABLE_UP: enableArrows
    val ESB_DISABLE_DOWN: enableArrows

    type SCROLLINFO =
        { minPos: int, maxPos: int, pageSize: int, pos: int, trackPos: int }

    datatype ScrollInfoOption =
        SIF_RANGE | SIF_PAGE | SIF_POS | SIF_DISABLENOSCROLL | SIF_TRACKPOS

    val SIF_ALL : ScrollInfoOption list

    datatype ScrollBarType = SB_BOTH | SB_CTL | SB_HORZ | SB_VERT
    datatype ScrollWindowFlag = SW_ERASE | SW_INVALIDATE | SW_SCROLLCHILDREN

    val EnableScrollBar : HWND * ScrollBarType * enableArrows -> unit
    val GetScrollInfo : HWND * ScrollBarType * ScrollInfoOption list -> SCROLLINFO
    val GetScrollPos : HWND * ScrollBarType -> int
    val ScrollDC : HDC * int * int * RECT * RECT * HRGN -> RECT
    val ScrollWindow : HWND * int * int * RECT * RECT -> unit
    val ScrollWindowEx : HWND * int * int * RECT * RECT * HRGN * ScrollWindowFlag list -> RECT
    val SetScrollInfo :
        HWND * ScrollBarType * ScrollInfoOption list * SCROLLINFO * bool -> int
    val SetScrollPos : HWND * ScrollBarType * int * bool -> int
    val SetScrollRange : HWND * ScrollBarType * int * int * bool -> bool
    val ShowScrollBar : HWND * ScrollBarType * bool -> unit
end
=
struct
    local
        open Foreign Base
    in
        open ScrollBase
        type HDC = HDC and HWND = HWND and HRGN = HRGN and RECT = RECT
    
        structure Style =
        struct
            open Window.Style (* Include all the windows styles. *)
    
            val SBS_HORZ                    = fromWord 0wx0000
            val SBS_VERT                    = fromWord 0wx0001
            val SBS_TOPALIGN                = fromWord 0wx0002
            val SBS_LEFTALIGN               = fromWord 0wx0002
            val SBS_BOTTOMALIGN             = fromWord 0wx0004
            val SBS_RIGHTALIGN              = fromWord 0wx0004
            val SBS_SIZEBOXTOPLEFTALIGN     = fromWord 0wx0002
            val SBS_SIZEBOXBOTTOMRIGHTALIGN = fromWord 0wx0004
            val SBS_SIZEBOX                 = fromWord 0wx0008
            val SBS_SIZEGRIP                = fromWord 0wx0010
    
            val all = flags[Window.Style.all, SBS_HORZ, SBS_VERT, SBS_TOPALIGN, SBS_BOTTOMALIGN,
                            SBS_SIZEBOX, SBS_SIZEGRIP]
    
            val intersect =
                List.foldl (fn (a, b) => fromWord(SysWord.andb(toWord a, toWord b))) all
        end
    
        datatype ScrollBarType = SB_CTL | SB_HORZ | SB_VERT | SB_BOTH
        
        local
            val tab = [
                (SB_HORZ,             0),
                (SB_VERT,             1),
                (SB_CTL,              2),
                (SB_BOTH,             3)
            ]
        in
            val cSCROLLBARTYPE = tableConversion(tab, NONE) cUint
                (* It's a UINT for EnableScrollBar and int for GetScrollInfo *)
        end
    
        datatype ScrollWindowFlag =
            SW_SCROLLCHILDREN | SW_INVALIDATE | SW_ERASE
        
        local
            val tab = [
                (SW_SCROLLCHILDREN,   0x0001),
                (SW_INVALIDATE,       0x0002),
                (SW_ERASE,            0x0004) ]
        in
            val cSCROLLWINDOWFLAG = tableSetConversion(tab, NONE) cUint
        end
    
    
        local
            open Foreign
            open Base
        in
            val EnableScrollBar = winCall3(user "EnableScrollBar") (cHWND, cSCROLLBARTYPE, cENABLESCROLLBARFLAG)
                    (successState "EnableScrollBar")
            val GetScrollPos = winCall2 (user "GetScrollPos") (cHWND,cSCROLLBARTYPE) cInt
            val SetScrollRange = winCall5(user "SetScrollRange") (cHWND,cSCROLLBARTYPE,cInt,cInt,cBool) cBool 
            val SetScrollPos = winCall4(user "SetScrollPos") (cHWND,cSCROLLBARTYPE,cInt,cBool) cInt
            val ShowScrollBar = winCall3(user "ShowScrollBar") (cHWND,cSCROLLBARTYPE,cBool) (successState "ShowScrollBar")
    
            val ScrollWindow = winCall5(user "ScrollWindow") (cHWND,cInt,cInt,cConstStar cRect,cConstStar cRect)
                    (successState "ScrollWindow")
    
            local
                val scrollDC =
                    winCall7 (user "ScrollDC") (cHDC,cInt,cInt,cConstStar cRect,cConstStar cRect,cHRGN,cStar cRect)
                        (successState "ScrollDC")
    
                val scrollWindowEx = winCall8(user "ScrollWindowEx")
                                             (cHWND,cInt,cInt,cConstStar cRect,cConstStar cRect,cHRGN,cStar cRect,cSCROLLWINDOWFLAG)
                                             (successState "ScrollWindowEx")
            in
                fun ScrollDC(hDC, dx, dy, prcScroll, prcClip, hrgnUpdate): RECT =
                let
                    val v = ref{top=0, bottom=0, left=0, right=0}
                    val () = scrollDC(hDC, dx, dy, prcScroll, prcClip, hrgnUpdate, v)
                in
                    ! v
                end
                and ScrollWindowEx(hWnd, dx, dy, prcScroll, prcClip, hrgnUpdate, flags) =
                let
                    val v = ref{top=0, bottom=0, left=0, right=0}
                    val () =
                        scrollWindowEx(hWnd, dx, dy, prcScroll, prcClip, hrgnUpdate, v, flags)
                in
                    ! v
                end
            end

            local
                val cSCROLLINFOSTRUCT as {ctype = {size=sizeStruct, ...}, ...} =
                    cStruct7(cUint, cSCROLLINFOOPTION, cInt, cInt, cUint, cInt, cInt)
                
                val getScrollInfo =
                    winCall3 (user "GetScrollInfo") (cHWND, cSCROLLBARTYPE, cStar cSCROLLINFOSTRUCT)
                                (successState "GetScrollInfo")
                and setScrollInfo =
                    winCall4 (user "SetScrollInfo") (cHWND, cSCROLLBARTYPE, cConstStar cSCROLLINFOSTRUCT, cBool) cInt
            in
                fun GetScrollInfo(hwnd, sbt, options): SCROLLINFO =
                let
                    val v = ref(Word.toInt sizeStruct, options, 0, 0, 0, 0, 0)
                    val _: unit = getScrollInfo(hwnd, sbt, v)
                    val (_, _, minPos, maxPos, pageSize, pos, trackPos) = ! v
                in
                    {minPos = minPos, maxPos = maxPos, pageSize = pageSize,
                      pos = pos, trackPos = trackPos}
                end
                
                and SetScrollInfo(hwnd, sbt, options,
                        { minPos, maxPos, pageSize, pos, trackPos}, redraw): int =
                    setScrollInfo(hwnd, sbt,
                        (Word.toInt sizeStruct, options, minPos, maxPos, pageSize, pos, trackPos), redraw)
            end
        end
    end
end;

(*
let
    open Scrollbar.Style

    (* The same values are used with different names for horizontal and vertical bars.
       Maybe we should generate different names according to whether the SBS_VERT flag
       is set. *)
    val flagTable =
        [(SBS_VERT,             "SBS_VERT"),
         (SBS_TOPALIGN,         "SBS_TOPALIGN"),
         (SBS_BOTTOMALIGN,      "SBS_BOTTOMALIGN"),
         (SBS_SIZEBOX,          "SBS_SIZEBOX"),
         (SBS_SIZEGRIP,         "SBS_SIZEGRIP"),
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
