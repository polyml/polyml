(*
    Copyright (c) 2001-7, 2015
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

structure Window :
sig
    type HWND and HINSTANCE and HMENU
    type POINT = { x: int, y: int }
    type RECT =  { left: int, top: int, right: int, bottom: int }

    structure Style:
    sig
        include BIT_FLAGS
        val WS_OVERLAPPED: flags and WS_POPUP: flags and WS_CHILD: flags and WS_MINIMIZE: flags
        and WS_VISIBLE: flags and WS_DISABLED:flags and WS_CLIPSIBLINGS:flags
        and WS_CLIPCHILDREN:flags and WS_MAXIMIZE:flags and WS_CAPTION:flags
        and WS_BORDER:flags and WS_DLGFRAME:flags and WS_VSCROLL:flags and WS_HSCROLL:flags
        and WS_SYSMENU:flags and WS_THICKFRAME:flags and WS_GROUP:flags and WS_TABSTOP:flags
        and WS_MINIMIZEBOX:flags and WS_MAXIMIZEBOX:flags and WS_TILED:flags and WS_ICONIC:flags
        and WS_SIZEBOX:flags and WS_OVERLAPPEDWINDOW:flags and WS_TILEDWINDOW:flags
        and WS_POPUPWINDOW:flags and WS_CHILDWINDOW:flags
    end
    
    structure ExStyle:
    sig
        include BIT_FLAGS
        val WS_EX_DLGMODALFRAME: flags and WS_EX_NOPARENTNOTIFY: flags and WS_EX_TOPMOST: flags
        and WS_EX_ACCEPTFILES : flags and WS_EX_TRANSPARENT: flags and WS_EX_MDICHILD: flags
        and WS_EX_TOOLWINDOW: flags and WS_EX_WINDOWEDGE: flags and WS_EX_CLIENTEDGE: flags
        and WS_EX_CONTEXTHELP: flags and WS_EX_RIGHT: flags and WS_EX_LEFT: flags
        and WS_EX_RTLREADING: flags and WS_EX_LTRREADING: flags and WS_EX_LEFTSCROLLBAR: flags
        and WS_EX_RIGHTSCROLLBAR: flags and WS_EX_CONTROLPARENT: flags and WS_EX_STATICEDGE: flags
        and WS_EX_APPWINDOW: flags and WS_EX_OVERLAPPEDWINDOW: flags and WS_EX_PALETTEWINDOW: flags
    end

    datatype WindowPositionStyle =
            SWP_ASYNCWINDOWPOS
        |   SWP_DEFERERASE
        |   SWP_FRAMECHANGED
        |   SWP_HIDEWINDOW
        |   SWP_NOACTIVATE
        |   SWP_NOCOPYBITS
        |   SWP_NOMOVE
        |   SWP_NOOWNERZORDER
        |   SWP_NOREDRAW
        |   SWP_NOSENDCHANGING
        |   SWP_NOSIZE
        |   SWP_NOZORDER
        |   SWP_SHOWWINDOW
        |   SWP_OTHER of Word32.word

    datatype ShowWindowOptions =
        SW_HIDE
    |   SW_MAXIMIZE
    |   SW_MINIMIZE
    |   SW_RESTORE
    |   SW_SHOW
    |   SW_SHOWDEFAULT
    |   SW_SHOWMAXIMIZED
    |   SW_SHOWMINIMIZED
    |   SW_SHOWMINNOACTIVE
    |   SW_SHOWNA
    |   SW_SHOWNOACTIVATE
    |   SW_SHOWNORMAL

    val SW_NORMAL: ShowWindowOptions
    val SW_MAX: ShowWindowOptions

    val ShowWindow: HWND * ShowWindowOptions -> bool

    datatype GetWindowFlags =
        GW_CHILD
    |   GW_HWNDFIRST
    |   GW_HWNDLAST
    |   GW_HWNDNEXT
    |   GW_HWNDPREV
    |   GW_OWNER

    datatype ParentType =
          ChildWindow of {id: int, parent: HWND}
        | PopupWindow of HMENU
        | PopupWithClassMenu

    val GWL_EXSTYLE : int
    val GWL_HINSTANCE : int
    val GWL_HWNDPARENT : int
    val GWL_ID : int
    val GWL_STYLE : int
    val GWL_USERDATA : int

    val AdjustWindowRect : RECT * Style.flags * bool -> RECT
    val AdjustWindowRectEx :  RECT * Style.flags * bool * int -> RECT
    val ArrangeIconicWindows : HWND -> int
    val BringWindowToTop : HWND -> unit
    val CW_USEDEFAULT : int
    val ChildWindowFromPoint : HWND * POINT -> HWND option
    val CloseWindow : HWND -> unit
    val CreateWindow :
       {x: int, y: int, init: 'a, name: string, class: 'a Class.ATOM,
         style: Style.flags, width: int, height: int,
         instance: HINSTANCE, relation: ParentType} -> HWND
    val CreateWindowEx :
       {x: int, y: int, init: 'a, name: string, class: 'a Class.ATOM,
         style: Style.flags, width: int, height: int,
         instance: HINSTANCE, relation: ParentType, exStyle: ExStyle.flags} -> HWND
    val CreateMDIClient: {
            relation: ParentType, style: Style.flags, instance: HINSTANCE, windowMenu: HMENU,
            idFirstChild: int} -> HWND
    val DefWindowProc: HWND * Message.Message -> Message.LRESULT
    val DefFrameProc: HWND * HWND * Message.Message -> Message.LRESULT
    val DefMDIChildProc: HWND * Message.Message -> Message.LRESULT
    val DestroyWindow: HWND -> unit
    val FindWindow: string option * string option -> HWND
    val FindWindowEx: HWND option * HWND option * string option * string option -> HWND
    val GetClassName : HWND -> string
    val GetClientRect : HWND -> RECT
    val GetDesktopWindow : unit -> HWND
    val GetForegroundWindow : unit -> HWND
    val GetLastActivePopup : HWND -> HWND
    val GetNextWindow : HWND * GetWindowFlags -> HWND
    val GetParent : HWND -> HWND option
    val GetTopWindow : HWND option -> HWND option
    val GetWindow : HWND * GetWindowFlags -> HWND option
    val GetWindowContextHelpId : HWND -> int
    val GetWindowLongPtr : HWND * int -> int
    val GetWindowRect : HWND -> RECT
    val GetWindowText : HWND -> string
    val GetWindowTextLength : HWND -> int
    val IsChild : HWND * HWND -> bool
    val IsIconic : HWND -> bool
    val IsWindow : HWND -> bool
    val IsWindowVisible : HWND -> bool
    val IsZoomed : HWND -> bool
    val MoveWindow : {x: int, y: int, hWnd: HWND, width: int, height: int, repaint: bool} -> unit
    val OpenIcon : HWND -> unit
    val SetForegroundWindow : HWND -> bool
    val SetParent : HWND * HWND option -> HWND
    val SetWindowContextHelpId : HWND * int -> unit
    val SetWindowLongPtr : HWND * int * int -> int
    val SetWindowPos : HWND * HWND * int * int * int * int * WindowPositionStyle list -> unit
    val SetWindowText : HWND * string -> unit
    val SubclassWindow :
       HWND *
       (HWND * Message.Message * 'a -> Message.LRESULT * 'a) * 'a ->
           (HWND  * Message.Message) -> Message.LRESULT
    val WindowFromPoint : POINT -> HWND option

end =
struct
local
    open Foreign
    open Globals
    open Base
    open Resource
    open Class

    fun checkWindow c = (checkResult(not(isHNull c)); c)
in
    type HWND = HWND and HINSTANCE = HINSTANCE and RECT = RECT and POINT = POINT
    and HMENU = HMENU

    open WinBase (* Get Style and SetWindowPositionStyle *)

    datatype ShowWindowOptions =
        SW_HIDE
    |   SW_MAXIMIZE
    |   SW_MINIMIZE
    |   SW_RESTORE
    |   SW_SHOW
    |   SW_SHOWDEFAULT
    |   SW_SHOWMAXIMIZED
    |   SW_SHOWMINIMIZED
    |   SW_SHOWMINNOACTIVE
    |   SW_SHOWNA
    |   SW_SHOWNOACTIVATE
    |   SW_SHOWNORMAL

    val SW_NORMAL = SW_SHOWNORMAL
    val SW_MAX = SW_SHOWDEFAULT

    local
        val showWindow = winCall2 (user "ShowWindow")(cHWND,cInt) (cBool)
    in
        fun ShowWindow (win, opt) = 
        let
            val cmd =
                case opt of
                    SW_HIDE             => 0
                |   SW_SHOWNORMAL       => 1
                |   SW_SHOWMINIMIZED    => 2
                |   SW_SHOWMAXIMIZED    => 3
                |   SW_MAXIMIZE         => 3
                |   SW_SHOWNOACTIVATE   => 4
                |   SW_SHOW             => 5
                |   SW_MINIMIZE         => 6
                |   SW_SHOWMINNOACTIVE  => 7
                |   SW_SHOWNA           => 8
                |   SW_RESTORE          => 9
                |   SW_SHOWDEFAULT      => 10
    
        in
            showWindow (win, cmd)
        end
    end

    val CloseWindow =
        winCall1 (user "CloseWindow") (cHWND) (successState "CloseWindow")
    val FindWindow =
        checkWindow o
        winCall2 (user "FindWindowA") (STRINGOPT, STRINGOPT) cHWND
    val FindWindowEx =
        checkWindow o
        winCall4 (user "FindWindowExA") (cHWNDOPT, cHWNDOPT, STRINGOPT, STRINGOPT) cHWND
    val GetDesktopWindow       = winCall0 (user "GetDesktopWindow") () cHWND
    val GetForegroundWindow    = winCall0 (user "GetForegroundWindow") () cHWND
    val GetLastActivePopup     = winCall1 (user "GetLastActivePopup") cHWND cHWND
    val GetParent              = winCall1 (user "GetParent") cHWND cHWNDOPT
    val GetTopWindow           = winCall1 (user "GetTopWindow") cHWNDOPT cHWNDOPT

    val GetWindowTextLength    = winCall1 (user "GetWindowTextLengthA") cHWND cInt
    val SetWindowText          =
        winCall2 (user "SetWindowTextA") (cHWND, cString) (successState "SetWindowText")

    local
        val getTextCall = winCall3 (user "GetWindowTextA") (cHWND, cPointer, cInt) cInt
    in
        fun GetWindowText(hwnd: HWND): string =
        let
            val baseLen = GetWindowTextLength hwnd
            (* The length returned by GetWindowTextLength may be larger than the text
               but we have to add one for the terminating null. *)
               open Memory
            val buff = malloc (Word.fromInt(baseLen+1))
            val size = getTextCall(hwnd, buff, baseLen+1)
        in
            (if size = 0 then ""
            else fromCstring buff) before free buff
        end
    end

    (* Get the class name of a window. *)
    local
        val getClassName = winCall3 (user "GetClassNameA") (cHWND, cPointer, cInt) cInt
    in
        (* Unfortunately we can't pass NULL here to get the length. *)
        fun GetClassName hwnd =
            getStringCall(fn (v, i) => getClassName(hwnd, v, i))
    end

    datatype GetWindowFlags =
        GW_CHILD
    |   GW_HWNDFIRST
    |   GW_HWNDLAST
    |   GW_HWNDNEXT
    |   GW_HWNDPREV
    |   GW_OWNER

    local
        fun winFlag GW_HWNDFIRST        = 0
        |   winFlag GW_HWNDLAST         = 1
        |   winFlag GW_HWNDNEXT         = 2
        |   winFlag GW_HWNDPREV         = 3
        |   winFlag GW_OWNER            = 4
        |   winFlag GW_CHILD            = 5

        val getWindow = winCall2 (user "GetWindow") (cHWND, cUint) cHWNDOPT
        val getNextWindow = winCall2 (user "GetNextWindow") (cHWND,cUint) cHWND
    in
        fun GetWindow (win, gwFlag) = getWindow (win, winFlag gwFlag)
        (* Only GW_HWNDNEXT and GW_HWNDPREV are allowed here but it's probably not
           worth making it a special case. *)
        fun GetNextWindow(win: HWND, gwFlag) =
            checkWindow (getNextWindow (win, winFlag gwFlag))
    end

    val IsChild                = winCall2 (user "IsChild") (cHWND,cHWND) cBool
    val IsIconic               = winCall1 (user "IsIconic") (cHWND) cBool
    val IsWindow               = winCall1 (user "IsWindow") (cHWND) cBool
    val IsWindowVisible        = winCall1 (user "IsWindowVisible") (cHWND) cBool
    val IsZoomed               = winCall1 (user "IsZoomed") (cHWND) cBool

    local
        val getClientRect = winCall2 (user "GetClientRect") (cHWND, cStar cRect) cBool
        and getWindowRect = winCall2 (user "GetWindowRect") (cHWND, cStar cRect) cBool
        and adjustWindowRect = winCall3 (user "AdjustWindowRect") (cStar cRect, cDWORD, cBool) cBool
        and adjustWindowRectEx = winCall4 (user "AdjustWindowRectEx") (cStar cRect, cDWORD, cBool, cDWORD) cBool
    in
        fun GetClientRect(hWnd: HWND): RECT =
        let
            val v =  ref{bottom=0, top=0, left=0, right=0}
            val res = getClientRect (hWnd, v)
        in
            checkResult res;
            !v
        end

        fun GetWindowRect(hWnd: HWND): RECT =
        let
            val v =  ref{bottom=0, top=0, left=0, right=0}
            val res = getWindowRect (hWnd, v)
        in
            checkResult res;
            !v
        end

        fun AdjustWindowRect(rect: RECT, style: Style.flags, bMenu: bool): RECT =
        let
            val v = ref rect
            val res = adjustWindowRect(v, LargeWord.toInt(Style.toWord style), bMenu)
        in
            checkResult res;
            !v
        end

        fun AdjustWindowRectEx(rect: RECT, style: Style.flags, bMenu: bool, exStyle: int): RECT =
        let
            val v = ref rect
            val res = adjustWindowRectEx(v, LargeWord.toInt(Style.toWord style), bMenu, exStyle)
        in
            checkResult res;
            !v
        end
    end

    val ArrangeIconicWindows = winCall1 (user "ArrangeIconicWindows") (cHWND) cUint
    val BringWindowToTop =
        winCall1 (user "BringWindowToTop") (cHWND) (successState "BringWindowToTop")
    val OpenIcon = winCall1 (user "OpenIcon") (cHWND) (successState "OpenIcon")
    val SetForegroundWindow = winCall1 (user "SetForegroundWindow") (cHWND) cBool

    local
        val setParent = winCall2 (user "SetParent") (cHWND, cHWND) cHWND
    in
        fun SetParent(child: HWND, new: HWND option): HWND =
        let
            val old = setParent(child, getOpt(new, hwndNull))
        in
            checkResult(not(isHNull old));
            old
        end
    end

    local
        val createWindowEx = 
            winCall12 (user "CreateWindowExA") (cDWORD, cString, cString, cDWORD, cInt, cInt, cInt, cInt,
                    cHWND, cPointer, cHINSTANCE, cPointer) cHWND
    in
        fun CreateWindowEx{class: 'a Class.ATOM, (* Window class *)
                         name: string, (* Window name *)
                         style: Style.flags, (* window style *)
                         exStyle: ExStyle.flags, (* extended style *)
                         x: int, (* horizontal position of window *)
                         y: int, (* vertical position of window *)
                         width: int, (* window width *)
                         height: int, (* window height *)
                         relation: ParentType, (* parent or owner window *)
                         instance: HINSTANCE, (* application instance *)
                         init: 'a}: HWND =
        let
            (* Set up a winCallback for ML classes and return the class name. *)
            val className: string =
                case class of
                    Registered { proc, className} =>
                        (Message.setCallback(proc, init);  className)
                |   SystemClass s => s

            val (parent, menu, styleWord) = WinBase.unpackWindowRelation(relation, style)

            (* Create a window. *)
            val res = createWindowEx
                    (LargeWord.toInt(ExStyle.toWord exStyle), className, name, LargeWord.toInt styleWord,
                     x, y, width, height, parent, menu, instance, Memory.null)
        in
            checkResult(not(isHNull res));
            res
        end
    end

    fun CreateWindow{class: 'a Class.ATOM, name: string, style: Style.flags, x: int,
                     y: int, width: int, height: int, relation: ParentType, instance: HINSTANCE,
                     init: 'a}: HWND =
        CreateWindowEx{exStyle=ExStyle.flags[], class=class, name=name, style=style, x=x,
                       y=y, width=width, height=height,relation=relation, instance=instance,
                       init=init}

    local
        val cCLIENTCREATESTRUCT = cStruct2(cHMENU, cUint)
        val createMDIClient =
            winCall12 (user "CreateWindowExA") (cDWORD, cString, cPointer, cDWORD, cInt, cInt, cInt, cInt,
                    cHWND, cPointer, cHINSTANCE, cConstStar cCLIENTCREATESTRUCT) cHWND
    in
        fun CreateMDIClient{
                relation: ParentType, (* This should always be ChildWindow *)
                style: Style.flags,
                instance: HINSTANCE,  (* application instance *)
                windowMenu: HMENU,    (* Window menu to which children are added. *)
                idFirstChild: int     (* Id of first child when it's created. *)
                }: HWND =
        let
            val (parent, menu, styleWord) =
                unpackWindowRelation(relation, style)
            val createS = (windowMenu, idFirstChild)
            val res = createMDIClient
                    (0, "MDICLIENT", Memory.null, LargeWord.toInt styleWord, 0, 0, 0, 0, parent, menu,
                     instance, createS)
        in
            checkResult(not(isHNull res));
            res
        end
    end

    local
        val defWindowProc =
                winCall4 (user "DefWindowProcA") (cHWND, cUint, cPointer, cPointer) cPointer
        and defFrameProc =
            winCall5 (user "DefFrameProcA") (cHWND, cHWND, cUint, cPointer, cPointer) cPointer
        and defMDIChildProc =
            winCall4 (user "DefMDIChildProcA") (cHWND, cUint, cPointer, cPointer) cPointer
    in 
        fun DefWindowProc (hWnd: HWND, msg: Message.Message): Message.LRESULT  =
        let
            val (wMsg, wParam, lParam) = Message.compileMessage msg
            val res = defWindowProc(hWnd, wMsg, wParam, lParam)
        in
            Message.messageReturnFromParams(msg, wParam, lParam, res)
        end

        fun DefFrameProc (hWnd: HWND, hWndMDIClient: HWND, msg: Message.Message): Message.LRESULT  =
        let
            val (wMsg, wParam, lParam) = Message.compileMessage msg
            val res = defFrameProc(hWnd, hWndMDIClient, wMsg, wParam, lParam)
        in
            (* Write back any changes the function has made. *)
            Message.messageReturnFromParams(msg, wParam, lParam, res)
        end

        fun DefMDIChildProc (hWnd: HWND, msg: Message.Message): Message.LRESULT =
        let
            val (wMsg, wParam, lParam) = Message.compileMessage msg
            val res = defMDIChildProc(hWnd, wMsg, wParam, lParam)
        in
            Message.messageReturnFromParams(msg, wParam, lParam, res)
        end
    end

    val CW_USEDEFAULT = ~0x80000000 (* Default value for size and/ot position. *)

    local
        val destroyWindow = winCall1 (user "DestroyWindow") (cHWND) (successState "DestroyWindow")
    in
        fun DestroyWindow(hWnd: HWND) =
        (
            destroyWindow hWnd;
            Message.removeCallback hWnd
        )
    end

    (*val GWL_WNDPROC         = ~4*)
    val GWL_HINSTANCE       = ~6
    val GWL_HWNDPARENT      = ~8
    val GWL_STYLE           = ~16
    val GWL_EXSTYLE         = ~20
    val GWL_USERDATA        = ~21
    val GWL_ID              = ~12

    val GetWindowLongPtr = winCall2 (user "GetWindowLongPtrA") (cHWND, cInt) cLONG_PTR

    (* SetWindowLong is a dangerous function to export. *)
    val SetWindowLongPtr = winCall3 (user "SetWindowLongPtrA") (cHWND, cInt, cLONG_PTR) cLONG_PTR

    (* ML extension.  This replaces the GetWindowLong and SetWindowLong calls. *)
    val SubclassWindow = Message.subclass

    local
        val moveWindow =
            winCall6(user "MoveWindow") (cHWND,cInt,cInt,cInt,cInt,cBool) (successState "MoveWindow")
    in
        fun MoveWindow{hWnd: HWND, x: int, y: int, height: int, width: int, repaint: bool} =
                moveWindow(hWnd, x, y, width, height, repaint)
    end

    val SetWindowPos = winCall7 (user "SetWindowPos")
        (cHWND, cHWND, cInt, cInt, cInt, cInt, cWINDOWPOSITIONSTYLE)
            (successState "SetWindowPos")

    val SetWindowContextHelpId =
            winCall2 (user "SetWindowContextHelpId") (cHWND, cDWORD)
                (successState "SetWindowContextHelpId")

    val GetWindowContextHelpId = winCall1 (user "GetWindowContextHelpId") (cHWND) cDWORD

    val ChildWindowFromPoint =
        winCall2 (user "ChildWindowFromPoint") (cHWND, cPoint) cHWNDOPT
    and WindowFromPoint =
        winCall1 (user "WindowFromPoint") (cPoint) cHWNDOPT
(*
TODO:
AnimateWindow    - Only Win98/NT 5.0
BeginDeferWindowPos  
CascadeWindows  
ChildWindowFromPointEx  
DeferWindowPos  
EndDeferWindowPos  
EnumChildProc  
EnumChildWindows  
EnumThreadWindows  
EnumThreadWndProc  
EnumWindows  
EnumWindowsProc  
GetWindowPlacement        
GetWindowThreadProcessId        
IsWindowUnicode                
SetWindowPlacement    
ShowOwnedPopups    
ShowWindowAsync  
TileWindows  
*)

end
end;

(* Because we're using opaque matching we have to install pretty printers
   outside the structure. *)
local
    open Window.Style

    val flagTable =
        [(WS_POPUP,             "WS_POPUP"),
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

    structure FlagP = FlagPrint(structure BITS = Window.Style)
in
    val _ = PolyML.addPrettyPrinter (FlagP.createFlagPrinter flagTable)
end;
