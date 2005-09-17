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

(* This contains the types used in the Win structure.  *)
structure WinBase =
struct
	local
		open CInterface Base
	in

		structure Style :>
		sig
			include BIT_FLAGS
		    val WS_BORDER : flags
		    val WS_CAPTION : flags
		    val WS_CHILD : flags
		    val WS_CHILDWINDOW : flags
		    val WS_CLIPCHILDREN : flags
		    val WS_CLIPSIBLINGS : flags
		    val WS_DISABLED : flags
		    val WS_DLGFRAME : flags
		    val WS_GROUP : flags
		    val WS_HSCROLL : flags
		    val WS_ICONIC : flags
		    val WS_MAXIMIZE : flags
		    val WS_MAXIMIZEBOX : flags
		    val WS_MINIMIZE : flags
		    val WS_MINIMIZEBOX : flags
		    val WS_OVERLAPPED : flags
		    val WS_OVERLAPPEDWINDOW : flags
		    val WS_POPUP : flags
		    val WS_POPUPWINDOW : flags
		    val WS_SIZEBOX : flags
		    val WS_SYSMENU : flags
		    val WS_TABSTOP : flags
		    val WS_THICKFRAME : flags
		    val WS_TILED : flags
		    val WS_TILEDWINDOW : flags
		    val WS_VISIBLE : flags
		    val WS_VSCROLL : flags
		end =
		struct
			type flags = SysWord.word
			fun toWord f = f
			fun fromWord f = f
			val flags = List.foldl (fn (a, b) => SysWord.orb(a,b)) 0w0
			fun allSet (fl1, fl2) = SysWord.andb(fl1, fl2) = fl1
			fun anySet (fl1, fl2) = SysWord.andb(fl1, fl2) <> 0w0
			fun clear (fl1, fl2) = SysWord.andb(SysWord.notb fl1, fl2)
	
			(* Window styles. *)
		    val WS_OVERLAPPED: flags						 = 0wx00000000
		    val WS_POPUP: flags                              = 0wx80000000
		    val WS_CHILD: flags                              = 0wx40000000
		    val WS_MINIMIZE: flags                           = 0wx20000000
		    val WS_VISIBLE: flags                            = 0wx10000000
		    val WS_DISABLED: flags                           = 0wx08000000
		    val WS_CLIPSIBLINGS: flags                       = 0wx04000000
		    val WS_CLIPCHILDREN: flags                       = 0wx02000000
		    val WS_MAXIMIZE: flags                           = 0wx01000000
		    val WS_CAPTION: flags                            = 0wx00C00000 (* WS_BORDER | WS_DLGFRAME *)
		    val WS_BORDER: flags                             = 0wx00800000
		    val WS_DLGFRAME: flags                           = 0wx00400000
		    val WS_VSCROLL: flags                            = 0wx00200000
		    val WS_HSCROLL: flags                            = 0wx00100000
		    val WS_SYSMENU: flags                            = 0wx00080000
		    val WS_THICKFRAME: flags                         = 0wx00040000
		    val WS_GROUP: flags                              = 0wx00020000
		    val WS_TABSTOP: flags                            = 0wx00010000
		    val WS_MINIMIZEBOX: flags                        = 0wx00020000
		    val WS_MAXIMIZEBOX: flags                        = 0wx00010000
		    val WS_TILED: flags                              = WS_OVERLAPPED
		    val WS_ICONIC: flags                             = WS_MINIMIZE
		    val WS_SIZEBOX: flags                            = WS_THICKFRAME
		    val WS_OVERLAPPEDWINDOW =
					flags[WS_OVERLAPPED, WS_CAPTION, WS_SYSMENU,
						  WS_THICKFRAME, WS_MINIMIZEBOX, WS_MAXIMIZEBOX]
		    val WS_TILEDWINDOW                               = WS_OVERLAPPEDWINDOW
		    val WS_POPUPWINDOW =
					flags[WS_POPUP, WS_BORDER, WS_SYSMENU]
		    val WS_CHILDWINDOW                               = WS_CHILD
	
			val all = flags[WS_OVERLAPPED, WS_POPUP, WS_CHILD, WS_MINIMIZE, WS_VISIBLE,
							WS_DISABLED, WS_CLIPSIBLINGS, WS_CLIPCHILDREN, WS_MAXIMIZE,
							WS_CAPTION, WS_BORDER, WS_DLGFRAME, WS_VSCROLL, WS_HSCROLL,
							WS_SYSMENU, WS_THICKFRAME, WS_GROUP, WS_TABSTOP, WS_MINIMIZEBOX,
							WS_MAXIMIZEBOX]
	
			val intersect = List.foldl (fn (a, b) => SysWord.andb(a,b)) all
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

		local
			val tab = [
			(SWP_NOSIZE,          0x0001),
			(SWP_NOMOVE,          0x0002),
			(SWP_NOZORDER,        0x0004),
			(SWP_NOREDRAW,        0x0008),
			(SWP_NOACTIVATE,      0x0010),
			(SWP_FRAMECHANGED,    0x0020),  (* The frame changed: send WM_NCCALCSIZE *)
			(SWP_SHOWWINDOW,      0x0040),
			(SWP_HIDEWINDOW,      0x0080),
			(SWP_NOCOPYBITS,      0x0100),
			(SWP_NOOWNERZORDER,   0x0200),  (* Don't do owner Z ordering *)
			(SWP_NOSENDCHANGING,  0x0400),  (* Don't send WM_WINDOWPOSCHANGING *)
			(SWP_DEFERERASE,      0x2000),
			(SWP_ASYNCWINDOWPOS,  0x4000)]

			(* It seems that some other bits are set although they're not defined. *)
			fun toInt (SWP_OTHER i) = i | toInt _ = raise Match
		in
			val WINDOWPOSITIONSTYLE = tableSetConversion(tab, SOME(SWP_OTHER, toInt))
		end
	
	end
end;
