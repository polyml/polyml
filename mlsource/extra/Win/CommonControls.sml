(*
    Copyright (c) 2007, 2015
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

(* Common controls. *)
structure CommonControls:
sig
    type HWND and HINSTANCE and HBITMAP
    val InitCommonControls: unit->unit
    
    structure ToolbarStyle:
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
        and TBSTYLE_BUTTON:flags and TBSTYLE_SEP:flags and TBSTYLE_CHECK:flags
        and TBSTYLE_GROUP:flags and TBSTYLE_CHECKGROUP:flags and TBSTYLE_DROPDOWN:flags
        and TBSTYLE_AUTOSIZE:flags and TBSTYLE_NOPREFIX:flags and TBSTYLE_TOOLTIPS:flags
        and TBSTYLE_WRAPABLE:flags and TBSTYLE_ALTDRAG:flags and TBSTYLE_FLAT:flags
        and TBSTYLE_LIST:flags and TBSTYLE_CUSTOMERASE:flags and TBSTYLE_REGISTERDROP:flags
        and TBSTYLE_TRANSPARENT:flags and BTNS_BUTTON:flags and BTNS_SEP:flags
        and BTNS_CHECK:flags and BTNS_GROUP:flags and BTNS_CHECKGROUP:flags
        and BTNS_DROPDOWN:flags and BTNS_AUTOSIZE:flags and BTNS_NOPREFIX:flags
        and BTNS_SHOWTEXT:flags and BTNS_WHOLEDROPDOWN:flags
    end
    
    structure ToolbarState:
    sig
        include BIT_FLAGS
        val TBSTATE_CHECKED: flags and TBSTATE_PRESSED: flags and TBSTATE_ENABLED: flags
        and TBSTATE_HIDDEN: flags and TBSTATE_INDETERMINATE: flags and TBSTATE_WRAP: flags
        and TBSTATE_ELLIPSES: flags and TBSTATE_MARKED : flags
    end

    datatype ToolbarResource =
        ToolbarHandle of HBITMAP | ToolbarResource of HINSTANCE*Resource.RESID
        
    datatype ParentType = datatype Window.ParentType

    type TBBUTTON = { iBitmap: int, idCommand: int, fsState: ToolbarState.flags,
                      fsStyle: ToolbarStyle.flags, dwData: int, isString: int};
    val CreateToolbarEx: { relation: ParentType, style: ToolbarStyle.flags, nBitmaps: int,
                           bitmaps: ToolbarResource, buttons: TBBUTTON list,
                           xButton: int, yButton: int, xBitmap: int, yBitmap: int} -> HWND
    val CreateStatusWindow: { relation: ParentType, style: Window.Style.flags, text: string } -> HWND
    
    val SB_SIMPLEID: int

    structure StatusBarType:
    sig
        include BIT_FLAGS
        val SBT_NOBORDERS: flags and SBT_OWNERDRAW: flags
        and SBT_POPOUT: flags and SBT_RTLREADING : flags and SBT_TOOLTIPS: flags
    end

    (* Creating messages here is just too complicated.  It's easier to do this with
       functions to send the message and deal with the result. *)
    val StatusBarSetText: {hWnd: HWND, iPart: int, uType: StatusBarType.flags, text: string}->int
    val StatusBarGetText: HWND*int -> string * StatusBarType.flags
    val StatusBarSetParts: HWND * int list -> bool
end =
struct
    datatype ParentType = datatype Window.ParentType

    local
        open Foreign
        open Globals
        open Base

    in
        type HWND = HWND and HINSTANCE = HINSTANCE and HBITMAP = HBITMAP

        val InitCommonControls = winCall0(comctl "InitCommonControls") () cVoid
        
        (* Toolbar style is a mess.  The TBBUTTON structure allows only a single
           byte for the style but some of the values exceed that.  Apparently
           it's necessary to use CreateWindowEx for those. *)
        structure ToolbarStyle =
        struct
            open Window.Style (* Include all the windows styles. *)
            val TBSTYLE_BUTTON      = fromWord 0wx0
            val TBSTYLE_SEP         = fromWord 0wx1
            val TBSTYLE_CHECK       = fromWord 0wx2
            val TBSTYLE_GROUP       = fromWord 0wx4
            val TBSTYLE_CHECKGROUP  = flags[TBSTYLE_GROUP,TBSTYLE_CHECK]
            val TBSTYLE_DROPDOWN    = fromWord 0wx8
            val TBSTYLE_AUTOSIZE    = fromWord 0wx10
            val TBSTYLE_NOPREFIX    = fromWord 0wx20
            val TBSTYLE_TOOLTIPS    = fromWord 0wx100
            val TBSTYLE_WRAPABLE    = fromWord 0wx200
            
            val TBSTYLE_ALTDRAG     = fromWord 0wx400
            
            val TBSTYLE_FLAT         = fromWord 0wx800
            val TBSTYLE_LIST         = fromWord 0wx1000
            val TBSTYLE_CUSTOMERASE  = fromWord 0wx2000
            val TBSTYLE_REGISTERDROP = fromWord 0wx4000
            val TBSTYLE_TRANSPARENT     = fromWord 0wx8000
            (* -- These are used with TB_SETEXTENDEDSTYLE/TB_GETEXTENDEDSTYLE
            val TBSTYLE_EX_DRAWDDARROWS = fromWord 0wx00000001
            val TBSTYLE_EX_MIXEDBUTTONS = fromWord 0w8
            val TBSTYLE_EX_HIDECLIPPEDBUTTONS = fromWord 0w16
            val TBSTYLE_EX_DOUBLEBUFFER = fromWord 0wx80*)
            val BTNS_BUTTON         = TBSTYLE_BUTTON
            val BTNS_SEP            = TBSTYLE_SEP
            val BTNS_CHECK          = TBSTYLE_CHECK
            val BTNS_GROUP          = TBSTYLE_GROUP
            val BTNS_CHECKGROUP     = TBSTYLE_CHECKGROUP
            val BTNS_DROPDOWN       = TBSTYLE_DROPDOWN
            val BTNS_AUTOSIZE       = TBSTYLE_AUTOSIZE
            val BTNS_NOPREFIX       = TBSTYLE_NOPREFIX
            val BTNS_SHOWTEXT       = fromWord 0wx0040
            val BTNS_WHOLEDROPDOWN  = fromWord 0wx0080

            val all = flags[Window.Style.all, TBSTYLE_BUTTON, TBSTYLE_SEP, TBSTYLE_CHECK,
                            TBSTYLE_GROUP, TBSTYLE_DROPDOWN, TBSTYLE_AUTOSIZE, TBSTYLE_NOPREFIX,
                            TBSTYLE_TOOLTIPS, TBSTYLE_WRAPABLE, TBSTYLE_ALTDRAG, TBSTYLE_FLAT,
                            TBSTYLE_LIST, TBSTYLE_CUSTOMERASE, TBSTYLE_TRANSPARENT,
                            BTNS_SHOWTEXT, BTNS_WHOLEDROPDOWN]
    
            val intersect =
                List.foldl (fn (a, b) => fromWord(SysWord.andb(toWord a, toWord b))) all
        end

        structure ToolbarState:>
        sig
            include BIT_FLAGS
            val TBSTATE_CHECKED: flags and TBSTATE_PRESSED: flags and TBSTATE_ENABLED: flags
            and TBSTATE_HIDDEN: flags and TBSTATE_INDETERMINATE: flags and TBSTATE_WRAP: flags
            and TBSTATE_ELLIPSES: flags and TBSTATE_MARKED : flags
            val cToolBarState: flags conversion (* Only used internally *)
        end =
        struct
            open Word8
            type flags = Word8.word
            val toWord = toLargeWord
            and fromWord = fromLargeWord
            val flags = List.foldl (fn (a, b) => orb(a,b)) 0w0
            fun allSet (fl1, fl2) = andb(fl1, fl2) = fl1
            fun anySet (fl1, fl2) = andb(fl1, fl2) <> 0w0
            fun clear (fl1, fl2) = andb(notb fl1, fl2)

            val TBSTATE_CHECKED         = 0w1
            val TBSTATE_PRESSED         = 0w2
            val TBSTATE_ENABLED         = 0w4
            val TBSTATE_HIDDEN          = 0w8
            val TBSTATE_INDETERMINATE   = 0wx10
            val TBSTATE_WRAP            = 0wx20
            val TBSTATE_ELLIPSES        = 0wx40
            val TBSTATE_MARKED          = 0wx80
            val all = flags[TBSTATE_CHECKED, TBSTATE_PRESSED, TBSTATE_ENABLED, TBSTATE_HIDDEN,
                            TBSTATE_INDETERMINATE, TBSTATE_WRAP, TBSTATE_ELLIPSES, TBSTATE_MARKED]
    
            val intersect = List.foldl (fn (a, b) => andb(a, b)) all
            
            val cToolBarState = cUint8w (*Must be a byte*)
        end

    
        datatype ToolbarResource =
            ToolbarHandle of HBITMAP | ToolbarResource of HINSTANCE*Resource.RESID
    
        type TBBUTTON = { iBitmap: int, idCommand: int, fsState: ToolbarState.flags,
                          fsStyle: ToolbarStyle.flags, dwData: int, isString: int}
        local
            val TBBUTTON =
                cStruct6(cInt, cInt, ToolbarState.cToolBarState(*byte*), cUint8w, cDWORD_PTR, cINT_PTR)
            val {ctype={size=sizeTBB, ...}, ...} = breakConversion TBBUTTON
                
            val createToolbarEx = winCall13 (comctl "CreateToolbarEx")
                (cHWND,cDWORDw,cUint,cInt,cHINSTANCE, cPointer ,cPointer,cInt,cInt,cInt,cInt,cInt,cUint) cHWND
            val list2vec = list2Vector TBBUTTON

        in
            fun CreateToolbarEx { relation: ParentType, style: ToolbarStyle.flags, nBitmaps: int,
                                  bitmaps: ToolbarResource, buttons: TBBUTTON list,
                                  xButton: int, yButton: int, xBitmap: int, yBitmap: int}: HWND =
            let
                (* This must be a child and WS_CHILD is included by default *)
                val (parent, childId, styleWord) =
                    case relation of
                        ChildWindow{parent, id} => (parent, id, WinBase.Style.toWord style)
                    |   _ => raise Fail "CreateToolbarEx: relation must be ChildWindow"

                fun mapToStruct({iBitmap, idCommand, fsState, fsStyle, dwData, isString}:TBBUTTON) =
                    (iBitmap, idCommand, fsState, Word8.fromLargeWord(ToolbarStyle.toWord fsStyle), dwData, isString)

                val (buttonVec, nButtons) = list2vec (map mapToStruct buttons)
                (* The wBMID argument may be either a resource identifier or a bitmap handle. *)
                val (hBMInst, wBMID, freeStr) =
                    case bitmaps of
                        ToolbarHandle hbm => (hinstanceNull, voidStarOfHandle hbm, Memory.null)
                    |   ToolbarResource(hi, IdAsInt wb) => (hi, Memory.sysWord2VoidStar(SysWord.fromInt wb), Memory.null)
                    |   ToolbarResource(hi, IdAsString str) => let val s = toCstring str in (hi, s, s) end

                val res =
                    createToolbarEx(parent, Word32.fromLargeWord styleWord, childId, nBitmaps,
                            hBMInst, wBMID, buttonVec, nButtons, xButton, yButton, xBitmap, yBitmap,
                            Word.toInt sizeTBB)
                        handle ex => (Memory.free freeStr; Memory.free buttonVec; raise ex)
                val () = Memory.free freeStr and () = Memory.free buttonVec
            in
                checkResult(not(isHNull res));
                res
            end
        end
        
        local
            val createStatusWindow = winCall4 (comctl "CreateStatusWindowA") (cLong,cString,cHWND,cUint) cHWND
        in
            fun CreateStatusWindow{ relation: ParentType, style: Window.Style.flags, text: string } =
            let
                val (parent, childId, styleWord) =
                    case relation of
                        ChildWindow{parent, id} =>
                            let open WinBase.Style in (parent, id, toWord(flags[WS_CHILD, style])) end
                    |   _ => raise Fail "CreateStatusWindow: relation must be ChildWindow"
                val res = createStatusWindow(LargeWord.toInt styleWord, text, parent, childId)
            in
                checkResult(not(isHNull res));
                res
            end
        end

        val SB_SIMPLEID = 0x00ff

        structure StatusBarType:
        sig
            include BIT_FLAGS
            val SBT_NOBORDERS: flags and SBT_OWNERDRAW: flags
            and SBT_POPOUT: flags and SBT_RTLREADING : flags and SBT_TOOLTIPS: flags
        end =
        struct
            type flags = SysWord.word
            fun toWord f = f
            fun fromWord f = f
            val flags = List.foldl (fn (a, b) => SysWord.orb(a,b)) 0w0
            fun allSet (fl1, fl2) = SysWord.andb(fl1, fl2) = fl1
            fun anySet (fl1, fl2) = SysWord.andb(fl1, fl2) <> 0w0
            fun clear (fl1, fl2) = SysWord.andb(SysWord.notb fl1, fl2)

            val SBT_NOBORDERS       = 0w256
            val SBT_OWNERDRAW       = 0wx1000
            val SBT_POPOUT          = 0w512
            val SBT_RTLREADING      = 0w1024
            val SBT_TOOLTIPS        = 0wx0800
            val all = flags[SBT_NOBORDERS, SBT_OWNERDRAW, SBT_POPOUT, SBT_RTLREADING, SBT_TOOLTIPS]
    
            val intersect =
                List.foldl (fn (a, b) => fromWord(SysWord.andb(toWord a, toWord b))) all
        end;
        
        val sendMsg = winCall4(user "SendMessageA") (cHWND, cUint, cUINT_PTR, cPointer) cUint

        fun StatusBarSetText{hWnd, iPart, uType, text}:int =
        let
            val s = toCstring text
            val res = sendMsg(hWnd, 0x401, IntInf.orb(iPart, LargeWord.toInt(StatusBarType.toWord uType)), s)
                handle ex => (Memory.free s; raise ex)
            val () = Memory.free s
        in
            res
        end
          
        fun StatusBarGetText(hWnd, iPart): string * StatusBarType.flags =
        let
            val result1 = Word32.fromInt(sendMsg(hWnd, 0x403, iPart, Memory.null))
            val length = LOWORD result1
            val flags = StatusBarType.fromWord(Word.toLargeWord(HIWORD result1))
        in
            if StatusBarType.anySet(flags, StatusBarType.SBT_OWNERDRAW)
            then ("", flags)
            else
            let
                open Memory
                val buff = malloc (length+0w1)
                val reply =
                    sendMsg(hWnd, 0x402, iPart, buff)
                        handle ex => (free buff; raise ex)
            in
                (if reply = 0 then "" else fromCstring buff, flags) before free buff
            end
        end

        fun StatusBarSetParts(hWnd, parts: int list): bool =
        let
            val (vec, nParts) = list2Vector cInt parts
            open Memory
            val res = sendMsg(hWnd, 0x404, nParts, vec)
                handle ex => (free vec; raise ex)
            val () = free vec
        in
            res <> 0
        end
          
 (* 

      | compileMessage (SB_GETTEXT { iPart: int, text: string ref, length: int }) =
            (* Another case, like LB_GETTEXT. where we don't know the length so we
               add an extra argument to the ML message. *)
            (0x402, toCint iPart, address(alloc (length+1) Cchar)*)


(*    | compileMessage (SB_SETTEXT { iPart: int, uType: StatusBarType, text: string}) =
              (0x401, toCint 0, toCstring text)
      | compileMessage (SB_GETTEXT _) = (0x402, toCint 0, toCInt 0)
      | compileMessage (SB_GETTEXTLENGTH _) = (0x403, toCint 0, toCInt 0)
      | compileMessage (SB_SETPARTS _) = (0x404, toCint 0, toCInt 0)
      | compileMessage (SB_GETPARTS _) = (0x406, toCint 0, toCInt 0)
      | compileMessage (SB_GETBORDERS _) = (0x407, toCint 0, toCInt 0)
      | compileMessage (SB_SETMINHEIGHT _) = (0x408, toCint 0, toCInt 0)
      | compileMessage (SB_SIMPLE _) = (0x409, toCint 0, toCInt 0)
      | compileMessage (SB_GETRECT _) = (0x40A, toCint 0, toCInt 0)*)

    end
end;
