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

	datatype WindowPositionStyle =
	    	SWP_ASYNCWINDOWPOS
	    |	SWP_DEFERERASE
	    |	SWP_FRAMECHANGED
	    |	SWP_HIDEWINDOW
	    |	SWP_NOACTIVATE
	    |	SWP_NOCOPYBITS
	    |	SWP_NOMOVE
	    |	SWP_NOOWNERZORDER
	    |	SWP_NOREDRAW
	    |	SWP_NOSENDCHANGING
	    |	SWP_NOSIZE
	    |	SWP_NOZORDER
	    |	SWP_SHOWWINDOW
		|	SWP_OTHER of int

	datatype ShowWindowOptions =
		SW_HIDE
	|	SW_MAXIMIZE
	|	SW_MINIMIZE
	|	SW_RESTORE
	|	SW_SHOW
	|	SW_SHOWDEFAULT
	|	SW_SHOWMAXIMIZED
	|	SW_SHOWMINIMIZED
	|	SW_SHOWMINNOACTIVE
	|	SW_SHOWNA
	|	SW_SHOWNOACTIVATE
	|	SW_SHOWNORMAL

	val SW_NORMAL: ShowWindowOptions
	val SW_MAX: ShowWindowOptions

	val ShowWindow: HWND * ShowWindowOptions -> bool

	datatype GetWindowFlags =
		GW_CHILD
	|	GW_HWNDFIRST
	|	GW_HWNDLAST
	|	GW_HWNDNEXT
	|	GW_HWNDPREV
	|	GW_OWNER

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
	val DestroyWindow: HWND -> unit
	val FindWindow: string * string option -> HWND
	val FindWindowEx: HWND option * HWND option * string * string option -> HWND
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
    val GetWindowLong : HWND * int -> int
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
    val SetWindowLong : HWND * int * int -> int
    val SetWindowPos : HWND * HWND * int * int * int * int * WindowPositionStyle list -> unit
    val SetWindowText : HWND * string -> unit
    val SubclassWindow :
       HWND *
	   (HWND * Message.Message * 'a -> Message.LRESULT option * 'a) * 'a -> unit
    val WindowFromPoint : POINT -> HWND option

end =
struct
local
	open CInterface
	open Globals
	open Base
	open Resource
	open Class

	fun checkWindow name c =
		if isHNull c then raise OS.SysErr(name, SOME(GetLastError())) else c
in
	type HWND = HWND and HINSTANCE = HINSTANCE and RECT = RECT and POINT = POINT
	and HMENU = HMENU

	open WinBase (* Get Style and SetWindowPositionStyle *)

	datatype ShowWindowOptions =
		SW_HIDE
	|	SW_MAXIMIZE
	|	SW_MINIMIZE
	|	SW_RESTORE
	|	SW_SHOW
	|	SW_SHOWDEFAULT
	|	SW_SHOWMAXIMIZED
	|	SW_SHOWMINIMIZED
	|	SW_SHOWMINNOACTIVE
	|	SW_SHOWNA
	|	SW_SHOWNOACTIVATE
	|	SW_SHOWNORMAL

	val SW_NORMAL = SW_SHOWNORMAL
	val SW_MAX = SW_SHOWDEFAULT

	fun ShowWindow (win, opt) = 
	let
		val cmd =
			case opt of
				SW_HIDE             => 0
			|	SW_SHOWNORMAL       => 1
			|	SW_SHOWMINIMIZED    => 2
			|	SW_SHOWMAXIMIZED    => 3
			|	SW_MAXIMIZE         => 3
			|	SW_SHOWNOACTIVATE   => 4
			|	SW_SHOW             => 5
			|	SW_MINIMIZE         => 6
			|	SW_SHOWMINNOACTIVE  => 7
			|	SW_SHOWNA           => 8
			|	SW_RESTORE          => 9
			|	SW_SHOWDEFAULT      => 10
	
	in
		call2 (user "ShowWindow")(HWND,INT) (BOOL) (win, cmd)
	end;

    val CloseWindow =
		call1 (user "CloseWindow") (HWND) (SUCCESSSTATE "CloseWindow")
	val FindWindow =
		checkWindow "FindWindow" o
		call2 (user "FindWindowA") (STRING, STRINGOPT) HWND
	val FindWindowEx =
		checkWindow "FindWindowEx" o
		call4 (user "FindWindowExA") (HWNDOPT, HWNDOPT, STRING, STRINGOPT) HWND
    val GetDesktopWindow       = call0 (user "GetDesktopWindow") () HWND
    val GetForegroundWindow    = call0 (user "GetForegroundWindow") () HWND
    val GetLastActivePopup     = call1 (user "GetLastActivePopup") HWND HWND
    val GetParent              = call1 (user "GetParent") HWND HWNDOPT
    val GetTopWindow           = call1 (user "GetTopWindow") HWNDOPT HWNDOPT

    val GetWindowTextLength    = call1 (user "GetWindowTextLengthA") HWND INT
	val SetWindowText		   =
		call2 (user "SetWindowTextA") (HWND, STRING) (SUCCESSSTATE "SetWindowText")

	fun GetWindowText(hwnd: HWND): string =
	let
		val getTextCall = call3 (user "GetWindowTextA") (HWND, POINTER, INT) INT
		val baseLen = GetWindowTextLength hwnd
		(* The length returned by GetWindowTextLength may be larger than the text
		   but we have to add one for the terminating null. *)
		val buff = alloc (baseLen+1) Cchar
		val size = getTextCall(hwnd, address buff, baseLen+1)
	in
		if size = 0 then ""
		else fromCstring(address buff)
	end

	(* Get the class name of a window.  The only way to do it is to loop until the
	   size returned is less than the size of the buffer. *)
	local
		val getClassCall =
			call3 (user "GetClassNameA") (HWND, POINTER, INT) (POSINT "GetClassName")
	in
		fun GetClassName(hwnd: HWND): string = 
			getStringCall(fn (buff, n) => getClassCall(hwnd, buff, n))
	end

	datatype GetWindowFlags =
		GW_CHILD
	|	GW_HWNDFIRST
	|	GW_HWNDLAST
	|	GW_HWNDNEXT
	|	GW_HWNDPREV
	|	GW_OWNER

	local
		fun winFlag GW_HWNDFIRST        = 0
		|	winFlag GW_HWNDLAST         = 1
		|	winFlag GW_HWNDNEXT         = 2
		|	winFlag GW_HWNDPREV         = 3
		|	winFlag GW_OWNER            = 4
		|	winFlag GW_CHILD            = 5
	in
		fun GetWindow (win, gwFlag) =
			call2 (user "GetWindow") (HWND,INT) HWNDOPT (win, winFlag gwFlag)
		(* Only GW_HWNDNEXT and GW_HWNDPREV are allowed here but it's probably not
		   worth making it a special case. *)
		fun GetNextWindow(win: HWND, gwFlag) =
			checkWindow "GetNextWindow" (
				call2 (user "GetNextWindow") (HWND,INT) HWND (win, winFlag gwFlag))
	end

    val IsChild                = call2 (user "IsChild") (HWND,HWND) BOOL
    val IsIconic               = call1 (user "IsIconic") (HWND) BOOL
    val IsWindow               = call1 (user "IsWindow") (HWND) BOOL
    val IsWindowVisible        = call1 (user "IsWindowVisible") (HWND) BOOL
    val IsZoomed               = call1 (user "IsZoomed") (HWND) BOOL

	fun GetClientRect(hWnd: HWND): RECT =
	let
		val buff = alloc 4 Clong
		val res = call2 (user "GetClientRect") (HWND, POINTER) BOOL (hWnd, address buff)
		val (toRect,_,_) = breakConversion RECT
	in
		if res
		then toRect buff
		else raise OS.SysErr("GetClientRect", SOME(GetLastError()))
	end

	fun GetWindowRect(hWnd: HWND): RECT =
	let
		val buff = alloc 4 Clong
		val res = call2 (user "GetWindowRect") (HWND, POINTER) BOOL (hWnd, address buff)
		val (toRect,_,_) = breakConversion RECT
	in
		if res
		then toRect buff
		else raise OS.SysErr("GetWindowRect", SOME(GetLastError()))
	end

	fun AdjustWindowRect(rect: RECT, style: Style.flags, bMenu: bool): RECT =
	let
		val (toRect,fromRect,_) = breakConversion RECT
		val buff = fromRect rect
		val res = call3 (user "AdjustWindowRect") (POINTER, INT, BOOL) BOOL
					(address buff, LargeWord.toInt(Style.toWord style), bMenu)
	in
		if res
		then toRect buff
		else raise OS.SysErr("AdjustWindowRect", SOME(GetLastError()))
	end

	fun AdjustWindowRectEx(rect: RECT, style: Style.flags, bMenu: bool, exStyle: int): RECT =
	let
		val (toRect,fromRect,_) = breakConversion RECT
		val buff = fromRect rect
		val res = call4 (user "AdjustWindowRectEx") (POINTER, INT, BOOL, INT) BOOL
					(address buff, LargeWord.toInt(Style.toWord style), bMenu, exStyle)
	in
		if res
		then toRect buff
		else raise OS.SysErr("AdjustWindowRectEx", SOME(GetLastError()))
	end

	val ArrangeIconicWindows = call1 (user "ArrangeIconicWindows") (HWND) INT (* POSINT? *)
    val BringWindowToTop =
		call1 (user "BringWindowToTop") (HWND) (SUCCESSSTATE "BringWindowToTop")
    val OpenIcon = call1 (user "OpenIcon") (HWND) (SUCCESSSTATE "OpenIcon")
    val SetForegroundWindow = call1 (user "SetForegroundWindow") (HWND) BOOL

	fun SetParent(child: HWND, new: HWND option): HWND =
	let
		val old = call2 (user "SetParent") (HWND, HWND) HWND (child, getOpt(new, hwndNull))
	in
		if isHNull old
		then raise OS.SysErr("SetParent", SOME(GetLastError()))
		else old
	end

	(* In C the parent and menu arguments are combined in a rather odd way. *)
	datatype ParentType =
		PopupWithClassMenu		(* Popup or overlapped window using class menu. *)
	|	PopupWindow of HMENU	(* Popup or overlapped window with supplied menu. *)
	|	ChildWindow of { parent: HWND, id: int } (* Child window. *)

	fun CreateWindow{class: 'a Class.ATOM, (* Window class *)
					 name: string, (* Window name *)
					 style: Style.flags, (* window style *)
					 x: int, (* horizontal position of window *)
					 y: int, (* vertical position of window *)
					 width: int, (* window width *)
					 height: int, (* window height *)
					 relation: ParentType, (* parent or owner window *)
					 instance: HINSTANCE, (* application instance *)
					 init: 'a}: HWND =
	let
		(* Get the ML callback function.  For system classes this is an identity fn. *)
		val (mlCallback, className: string) =
			case class of
				Registered { proc, className} => (proc, className)
			|	SystemClass s => (fn(_, _, a) => (NONE, a), s)

		(* In the case of a child window the "menu" is actually an integer
		   which identifies the child in notification messages to the parent.
		   We silently set or clear the WS_CHILD bit depending on the argument. *)
		val (parent, menu, styleWord) =
			case relation of
				PopupWithClassMenu =>
					(hwndNull, hmenuNull, Style.toWord(Style.clear(Style.WS_CHILD, style)))
			|	PopupWindow hm =>
					(hwndNull, hm, Style.toWord(Style.clear(Style.WS_CHILD, style)))
			|	ChildWindow{parent, id} =>
					(parent, handleOfInt id, Style.toWord(Style.flags[Style.WS_CHILD, style]))

		(* Make an entry in the table for this window. *)
		(* TEMPORARY.  TODO: Something better than using NULL here. *)
		val DefWindowProc = call4 (user "DefWindowProcA") (INT, INT, INT, INT) INT
		val _ = Message.addCallback(hwndNull, mlCallback, init, DefWindowProc);

		(* Create a window. *)
		val res =
			call12 (user "CreateWindowExA") (INT, STRING, STRING, WORD, INT, INT, INT, INT,
					HWND, HMENU, HINSTANCE, INT) HWND
				(0, className, name, styleWord, x, y, width, height, parent, menu,
				 instance, 0)
	in
		if isHNull res
		then raise OS.SysErr("CreateWindow", SOME(GetLastError())) else ();
		(* This is still messy.  If we have created a window of a system class we
		   won't have received anything in the callback function.  That means we'll
		   still have this wretched zero around and that will cause problems if
		   we then try to subclass it.  We need to explicitly update the entry
		   if it's still zero.
		   A better solution might be not to create an entry for a system class
		   UNTIL we subclass it. *)
		Message.updateWindowHandle res;
		res
	end

	val CW_USEDEFAULT = ~0x80000000 (* Default value for size and/ot position. *)

	fun DestroyWindow(hWnd: HWND) =
	(
		call1 (user "DestroyWindow") (HWND) (SUCCESSSTATE "DestroyWindow") hWnd;
		Message.removeCallback hWnd
	)

	(*val GWL_WNDPROC         = ~4*)
	val GWL_HINSTANCE       = ~6
	val GWL_HWNDPARENT      = ~8
	val GWL_STYLE           = ~16
	val GWL_EXSTYLE         = ~20
	val GWL_USERDATA        = ~21
	val GWL_ID              = ~12

	val GetWindowLong = call2 (user "GetWindowLongA") (HWND, INT) INT

	(* SetWindowLong is a dangerous function to export. *)
	val SetWindowLong = call3 (user "SetWindowLongA") (HWND, INT, INT) INT

 	(* ML extension.  This replaces the GetWindowLong and SetWindowLong calls. *)
	val SubclassWindow = Message.subclass

	fun MoveWindow{hWnd: HWND, x: int, y: int, height: int, width: int, repaint: bool} =
		call6(user "MoveWindow") (HWND,INT,INT,INT,INT,BOOL) (SUCCESSSTATE "MoveWindow")
			(hWnd, x, y, width, height, repaint)

	val SetWindowPos = call7 (user "SetWindowPos")
		(HWND, HWND, INT, INT, INT, INT, WINDOWPOSITIONSTYLE)
			(SUCCESSSTATE "SetWindowPos")

	val SetWindowContextHelpId =
			call2 (user "SetWindowContextHelpId") (HWND, INT)
				(SUCCESSSTATE "SetWindowContextHelpId")

	val GetWindowContextHelpId = call1 (user "GetWindowContextHelpId") (HWND) INT

	local
		(* The C interface currently passes structures by reference.  That's
		   certainly wrong for Microsoft C and I suspect it's also wrong on
		   Unix.  I'm reluctant to change it without knowing more.  DCJM. *)
		val childWindowFromPoint =
			call3 (user "ChildWindowFromPoint") (HWND, INT, INT) HWNDOPT
		and windowFromPoint =
			call2 (user "WindowFromPoint") (INT, INT) HWNDOPT
	in
		fun ChildWindowFromPoint(hw, {x, y}:POINT) = childWindowFromPoint(hw, x, y)
		and WindowFromPoint({x, y}:POINT) = windowFromPoint(x, y)
	end
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
		[(WS_POPUP, 			"WS_POPUP"),
	     (WS_CHILD,				"WS_CHILD"),
	     (WS_MINIMIZE,			"WS_MINIMIZE"),
	     (WS_VISIBLE,			"WS_VISIBLE"),
	     (WS_DISABLED,			"WS_DISABLED"),
	     (WS_CLIPSIBLINGS,		"WS_CLIPSIBLINGS"),
	     (WS_CLIPCHILDREN,		"WS_CLIPCHILDREN"),
	     (WS_MAXIMIZE,			"WS_MAXIMIZE"),
	     (WS_CAPTION,			"WS_CAPTION"),
	     (WS_BORDER,			"WS_BORDER"),
	     (WS_DLGFRAME,			"WS_DLGFRAME"),
	     (WS_VSCROLL,			"WS_VSCROLL"),
	     (WS_HSCROLL,			"WS_HSCROLL"),
	     (WS_SYSMENU,			"WS_SYSMENU"),
	     (WS_THICKFRAME,		"WS_THICKFRAME"),
	     (WS_GROUP,				"WS_GROUP"),
	     (WS_TABSTOP,			"WS_TABSTOP"),
	     (WS_MINIMIZEBOX,		"WS_MINIMIZEBOX"),
	     (WS_MAXIMIZEBOX,		"WS_MAXIMIZEBOX")]

	structure FlagP = FlagPrint(structure BITS = Window.Style)
in
	val _ = PolyML.install_pp (FlagP.createFlagPrinter flagTable)
end;
