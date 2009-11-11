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

(* This contains the types used in the Message structure.  *)
structure MessageBase =
struct
    local
        open CInterface Base
    in
        type flags = WinBase.Style.flags
        and WindowPositionStyle = WinBase.WindowPositionStyle

        datatype LRESULT =
            LRESINT of int
        |   LRESHANDLE of HGDIOBJ

        (* Maybe we should have two different types for horizontal and vertical. *)
        datatype ScrollDirection =
            SB_BOTTOM | SB_ENDSCROLL | SB_LINEDOWN | SB_LINEUP | SB_PAGEDOWN | SB_PAGEUP |
            SB_THUMBPOSITION | SB_THUMBTRACK | SB_TOP | SB_LEFT | SB_RIGHT | SB_LINELEFT |
            SB_LINERIGHT | SB_PAGELEFT | SB_PAGERIGHT
        local
            val tab = [
                (SB_LINEUP, 0),
                (SB_LINELEFT, 0),
                (SB_LINEDOWN, 1),
                (SB_LINERIGHT, 1),
                (SB_PAGEUP, 2),
                (SB_PAGELEFT, 2),
                (SB_PAGEDOWN, 3),
                (SB_PAGERIGHT, 3),
                (SB_THUMBPOSITION, 4),
                (SB_THUMBTRACK, 5),
                (SB_TOP, 6),
                (SB_LEFT, 6),
                (SB_BOTTOM, 7),
                (SB_RIGHT, 7),
                (SB_ENDSCROLL, 8)
                ]
        in
            val SCROLLDIRECTION = tableConversion(tab, NONE)
        end

        (* This is a bit of a mess.  Various operations take or return handles to
           these types of image and also take this value as a parameter. *)
        datatype ImageType = IMAGE_BITMAP | IMAGE_CURSOR | IMAGE_ENHMETAFILE | IMAGE_ICON
    
        local
            val tab = [
                (IMAGE_BITMAP, 0),
                (IMAGE_ICON, 1),
                (IMAGE_CURSOR, 2),
                (IMAGE_ENHMETAFILE, 3)
                ]
        in
            val IMAGETYPE = tableConversion(tab, NONE)
        end

        (* WM_SIZE options. *)
        datatype WMSizeOptions =
            SIZE_RESTORED | SIZE_MINIMIZED | SIZE_MAXIMIZED | SIZE_MAXSHOW | SIZE_MAXHIDE
        local
            val tab = [
                (SIZE_RESTORED,       0),
                (SIZE_MINIMIZED,      1),
                (SIZE_MAXIMIZED,      2),
                (SIZE_MAXSHOW,        3),
                (SIZE_MAXHIDE,        4)
                ]
        in
            val WMSIZEOPTIONS = tableConversion(tab, NONE)
        end

        (* WM_ACTIVATE options *)
        datatype WMActivateOptions = WA_INACTIVE | WA_ACTIVE | WA_CLICKACTIVE
        local
            val 
            tab = [
                (WA_INACTIVE,       0),
                (WA_ACTIVE,         1),
                (WA_CLICKACTIVE,    2)
                ]
        in
            val (fromWMactive, toWMactive) = tableLookup(tab, NONE)
        end

        datatype SystemCommand =
            SC_SIZE | SC_MOVE | SC_MINIMIZE | SC_MAXIMIZE | SC_NEXTWINDOW | SC_PREVWINDOW |
            SC_CLOSE | SC_VSCROLL | SC_HSCROLL | SC_MOUSEMENU | SC_KEYMENU | SC_ARRANGE |
            SC_RESTORE | SC_TASKLIST | SC_SCREENSAVE | SC_HOTKEY | SC_DEFAULT |
            SC_MONITORPOWER | SC_CONTEXTHELP | SC_SEPARATOR
        local
            val tab = [
                (SC_SIZE, 0xF000),
                (SC_MOVE, 0xF010),
                (SC_MINIMIZE, 0xF020),
                (SC_MAXIMIZE, 0xF030),
                (SC_NEXTWINDOW, 0xF040),
                (SC_PREVWINDOW, 0xF050),
                (SC_CLOSE, 0xF060),
                (SC_VSCROLL, 0xF070),
                (SC_HSCROLL, 0xF080),
                (SC_MOUSEMENU, 0xF090),
                (SC_KEYMENU, 0xF100),
                (SC_ARRANGE, 0xF110),
                (SC_RESTORE, 0xF120),
                (SC_TASKLIST, 0xF130),
                (SC_SCREENSAVE, 0xF140),
                (SC_HOTKEY, 0xF150),
                (SC_DEFAULT, 0xF160),
                (SC_MONITORPOWER, 0xF170),
                (SC_CONTEXTHELP, 0xF180)]
        in
            val (fromSysCommand, toSysCommand) = tableLookup(tab, NONE)
        end

        datatype ControlType = ODT_MENU | ODT_LISTBOX | ODT_COMBOBOX | ODT_BUTTON | ODT_STATIC
        local
            val 
            tab = [
                (ODT_MENU, 1),
                (ODT_LISTBOX, 2),
                (ODT_COMBOBOX, 3),
                (ODT_BUTTON, 4),
                (ODT_STATIC, 5)
                ]
        in
            val CONTROLTYPE = tableConversion(tab, NONE)
        end


        datatype WMPrintOption = 
            PRF_CHECKVISIBLE | PRF_NONCLIENT | PRF_CLIENT | PRF_ERASEBKGND |
            PRF_CHILDREN | PRF_OWNED

        local
            val tab = [
                (PRF_CHECKVISIBLE,      0x00000001),
                (PRF_NONCLIENT,         0x00000002),
                (PRF_CLIENT,            0x00000004),
                (PRF_ERASEBKGND,        0x00000008),
                (PRF_CHILDREN,          0x00000010),
                (PRF_OWNED,             0x00000020)
                ]
        in
            val WMPRINTOPS = tableSetConversion(tab, NONE)
        end

        (* Parameters to EM_SETMARGINS. *)
        datatype MarginSettings = 
            UseFontInfo | Margins of {left: int option, right: int option }

        datatype MouseKeyFlags = MK_LBUTTON | MK_RBUTTON | MK_SHIFT | MK_CONTROL | MK_MBUTTON

        local
            val tab = [
                (MK_LBUTTON,        0x0001),
                (MK_RBUTTON,        0x0002),
                (MK_SHIFT,          0x0004),
                (MK_CONTROL,        0x0008),
                (MK_MBUTTON,        0x0010)
                ]
        in
            val MOUSEKEYFLAGS = tableSetConversion(tab, NONE)
        end

        datatype MDITileFlags = MDITILE_VERTICAL | MDITILE_HORIZONTAL | MDITILE_SKIPDISABLED

        local
            val tab = [
                (MDITILE_VERTICAL,      0x0000),
                (MDITILE_HORIZONTAL,    0x0001),
                (MDITILE_SKIPDISABLED,  0x0002)
                ]
        in
            val MDITILEFLAGS = tableSetConversion(tab, NONE)
        end

        (* TODO: Perhaps use a datatype for this.  It's always possible to use
           functions from IntInf though. *)
        type KeyData = int

        datatype HelpHandle = MenuHandle of HMENU | WindowHandle of HWND

        (* Passed in the lpParam argument of a WM_NOTIFY message.
           TODO: Many of these have additional information. *)
        datatype Notification =
            NM_OUTOFMEMORY
        |   NM_CLICK
        |   NM_DBLCLK
        |   NM_RETURN
        |   NM_RCLICK
        |   NM_RDBLCLK
        |   NM_SETFOCUS
        |   NM_KILLFOCUS
        |   NM_CUSTOMDRAW
        |   NM_HOVER
        |   NM_NCHITTEST
        |   NM_KEYDOWN
        |   NM_RELEASEDCAPTURE
        |   NM_SETCURSOR
        |   NM_CHAR
        |   NM_TOOLTIPSCREATED
        |   NM_LDOWN
        |   NM_RDOWN
        |   NM_THEMECHANGED
        |   LVN_ITEMCHANGING
        |   LVN_ITEMCHANGED
        |   LVN_INSERTITEM
        |   LVN_DELETEITEM
        |   LVN_DELETEALLITEMS
        |   LVN_BEGINLABELEDIT
        |   LVN_ENDLABELEDIT
        |   LVN_COLUMNCLICK
        |   LVN_BEGINDRAG
        |   LVN_BEGINRDRAG
        |   LVN_GETDISPINFO
        |   LVN_SETDISPINFO
        |   LVN_KEYDOWN
        |   LVN_GETINFOTIP
        |   HDN_ITEMCHANGING
        |   HDN_ITEMCHANGED
        |   HDN_ITEMCLICK
        |   HDN_ITEMDBLCLICK
        |   HDN_DIVIDERDBLCLICK
        |   HDN_BEGINTRACK
        |   HDN_ENDTRACK
        |   HDN_TRACK
        |   HDN_ENDDRAG
        |   HDN_BEGINDRAG
        |   HDN_GETDISPINFO
        |   TVN_SELCHANGING
        |   TVN_SELCHANGED
        |   TVN_GETDISPINFO
        |   TVN_SETDISPINFO
        |   TVN_ITEMEXPANDING
        |   TVN_ITEMEXPANDED
        |   TVN_BEGINDRAG
        |   TVN_BEGINRDRAG
        |   TVN_DELETEITEM
        |   TVN_BEGINLABELEDIT
        |   TVN_ENDLABELEDIT
        |   TVN_KEYDOWN
        |   TVN_GETINFOTIP
        |   TVN_SINGLEEXPAND
        |   TTN_GETDISPINFO of string ref
        |   TTN_SHOW
        |   TTN_POP
        |   TCN_KEYDOWN
        |   TCN_SELCHANGE
        |   TCN_SELCHANGING
        |   TBN_GETBUTTONINFO
        |   TBN_BEGINDRAG
        |   TBN_ENDDRAG
        |   TBN_BEGINADJUST
        |   TBN_ENDADJUST
        |   TBN_RESET
        |   TBN_QUERYINSERT
        |   TBN_QUERYDELETE
        |   TBN_TOOLBARCHANGE
        |   TBN_CUSTHELP
        |   TBN_DROPDOWN
        |   TBN_HOTITEMCHANGE
        |   TBN_DRAGOUT
        |   TBN_DELETINGBUTTON
        |   TBN_GETDISPINFO
        |   TBN_GETINFOTIP
        |   UDN_DELTAPOS
        |   RBN_GETOBJECT
        |   RBN_LAYOUTCHANGED
        |   RBN_AUTOSIZE
        |   RBN_BEGINDRAG
        |   RBN_ENDDRAG
        |   RBN_DELETINGBAND
        |   RBN_DELETEDBAND
        |   RBN_CHILDSIZE
        |   CBEN_GETDISPINFO
        |   CBEN_DRAGBEGIN
        |   IPN_FIELDCHANGED
        |   SBN_SIMPLEMODECHANGE
        |   PGN_SCROLL
        |   PGN_CALCSIZE
        |   NM_OTHER of int (* Catch-all for other cases. *)

        datatype Message     =
                    WM_ACTIVATE of {active : WMActivateOptions, minimize : bool }
                      (* Indicates a change in activation state *)

                    | WM_ACTIVATEAPP of {active  : bool, threadid: int  } 
                      (* Notifies applications when a new task activates *)

                    | WM_ASKCBFORMATNAME of { length: int, formatName : string ref} 
                      (* Retrieves the name of the clipboard format *)

                    | WM_CANCELJOURNAL  
                      (* Notifies application when user cancels journaling *)

                    | WM_CANCELMODE 
                      (* Notifies a Window to cancel internal modes *)

                    | WM_CHANGECBCHAIN of { removed : HWND, next : HWND  }  
                      (* Notifies clipboard viewer of removal from chain *)

                    | WM_CHAR of {charCode : char, data : KeyData }                     
                      (* Indicates the user pressed a character key *)

                    | WM_CHARTOITEM of {key : int, caretpos : int, listbox  : HWND  }
                      (* Provides list-box keystrokes to owner Window *)

                    | WM_CHILDACTIVATE  
                      (* Notifies a child Window of activation *)

(* This is WM_USER+1.  It's only used in a GetFont dialogue box.
                   | WM_CHOOSEFONT_GETLOGFONT of LOGFONT ref *)
                      (* Retrieves LOGFONT structure for Font dialog box *)

                    | WM_CLEAR
                      (* Clears an edit control *)

                    | WM_CLOSE      
                      (* System Close menu command was chosen *)

                    | WM_COMMAND of {notifyCode : int,
                                  wId        : int,
                                  control    : HWND  }
                      (* Specifies a command message *)
                       
                    | WM_COMPACTING of { compactratio : int }
                      (* Indicates a low memory condition *)

                    | WM_COMPAREITEM of {controlid : int,
                                           ctlType : ControlType,
                                           ctlID : int,
                                           hItem : HWND,
                                           itemID1 : int,
                                           itemData1 : int,
                                           itemID2 : int,
                                           itemData2 : int                                        
                                           }
                      (* Determines position of combo- or list-box item *)

                    | WM_COPY
                      (* Copies a selection to the clipboard *)

                    | WM_COPYDATA of { sender : HWND, data : int, pdata: Word8Vector.vector }
                      (* Passes data to another application  *)

                    | WM_CREATE of { instance: HINSTANCE,
                                     creation: int,
                                     menu : HMENU,
                                     parent : HWND,
                                     cy : int,
                                     cx : int,
                                     y : int,
                                     x : int,
                                     style : flags,
                                     name : string,
                                     (* The class may be a string or an atom. *)
                                     class : ClassType,
                                     extendedstyle : int                                                                          
                                     }
                      (* Indicates a Window is being created *)

                    | WM_CTLCOLORBTN of { displaycontext : HDC, button : HWND  }
                      (* Button is about to be drawn *)

                    | WM_CTLCOLORDLG of { displaycontext : HDC,
                                       dialogbox      : HWND  }
                      (* Dialog box is about to be drawn *)
                     
                    | WM_CTLCOLOREDIT of {  displaycontext : HDC, editcontrol : HWND  }
                      (* Control is about to be drawn *)

                    | WM_CTLCOLORLISTBOX of { displaycontext : HDC, listbox : HWND   }
                      (* List box is about to be drawn *)

                    | WM_CTLCOLORMSGBOX of { displaycontext : HDC,
                                             messagebox     : HWND  }
                      (* Message box is about to be drawn *)

                    | WM_CTLCOLORSCROLLBAR of { displaycontext : HDC,
                                             scrollbar      : HWND  }
                      (* Indicates scroll bar is about to be drawn *)

                    | WM_CTLCOLORSTATIC of { displaycontext : HDC,
                                             staticcontrol  : HWND  }
                      (* Control is about to be drawn *)
                      (* Note the return value is an HBRUSH *)

                    | WM_CUT
                      (* Deletes a selection and copies it to the clipboard *)
(*
                    | WM_DDE_ACK
                      (* Acknowledges a DDE message *)

                    | WM_DDE_ADVISE 
                      (* Requests a DDE data-change update *)

                    | WM_DDE_DATA
                      (* Sends data to a DDE client *)
                       
                    | WM_DDE_EXECUTE
                      (* Sends a string to a DDE server *)

                    | WM_DDE_INITIATE   
                      (* Initiates a DDE conversation *)

                    | WM_DDE_POKE   
                      (* Send unsolicited data to a server *)
                       
                    | WM_DDE_REQUEST    
                      (* Requests data from a DDE server *)

                    | WM_DDE_TERMINATE  
                      (* Ends a DDE conversation *)

                    | WM_DDE_UNADVISE   
                      (* Stops a DDE data-update request *)
*)

                    | WM_DEADCHAR of { charCode : char, data  : KeyData   }
                      (* Indicates the user pressed a dead key *)

                    | WM_DELETEITEM of { controlid : int,
                                             ctlType : ControlType,
                                             ctlID : int,
                                             itemID : int,
                                             item : HWND,
                                             itemData : int                                         
                                           }
                      (* Indicates owner-draw item or control was altered *)

                    | WM_DESTROY    
                      (* Indicates Window is about to be destroyed *)

                    | WM_DESTROYCLIPBOARD   
                      (* Notifies owner that the clipboard was emptied *)
                     
                    | WM_DEVMODECHANGE of { devicename : string }   
                      (* Indicates the device-mode settings have changed *)
                     
                    | WM_DRAWCLIPBOARD  
                      (* Indicates the clipboard's contents have changed *) 
                    
                    | WM_DRAWITEM of { controlid : int,
                                       ctlType : ControlType,
                                       ctlID : int,
                                       itemID : int,
                                       itemAction : int,
                                       itemState : int,
                                       hItem : HWND ,
                                       hDC : HDC,
                                       rcItem : RECT,
                                       itemData : int
                                        }   
                      (* Indicates owner-draw control/menu needs redrawing *) 
                     
                    | WM_DROPFILES of { hDrop : HDROP } 
                      (* Indicates that a file has been dropped *)
                    
                    | WM_ENABLE of { enabled : bool }
                      (* Indicates a Window's enable state is changing *)
                    
                    | WM_ENDSESSION of { endsession : bool }
                      (* Indicates whether the Windows session is ending *)
                    
                    | WM_ENTERIDLE of { flag : int, window : HWND }
                      (* Indicates a modal dialog box or menu is idle *)
                    
                    | WM_ENTERMENULOOP of { istrack: bool }
                      (* Indicates entry into menu modal loop *)
                    
                    | WM_EXITMENULOOP of { istrack: bool }
                      (* Indicates exit from menu modal loop *)
                    
                    | WM_ERASEBKGND of { devicecontext : HDC }
                      (* Indicates a Window's background need erasing *)
                    
                    | WM_FONTCHANGE
                      (* Indicates a change in the font-resource pool *)
                     
                    | WM_GETDLGCODE
                      (* Allows dialog procedure to process control input *)
                    
                    | WM_GETFONT    
                      (* Retrieves the font that a control is using *)
                    
                    | WM_GETHOTKEY
                      (* Gets the virtual-key code of a Window's hot key *) 
                    
                    | WM_GETMINMAXINFO of
                         { maxSize: POINT ref, maxPosition: POINT ref,
                           minTrackSize: POINT ref, maxTrackSize : POINT ref }
                      (* Gets minimum and maximum sizing information *)
                    
                    | WM_GETTEXT of { length: int, text : string ref  } 
                      (* Gets the text that corresponds to a Window *)
                    
                    | WM_GETTEXTLENGTH  
                      (* Gets length of text associated with a Window *)
                    
                    | WM_HOTKEY of { id : int }
                      (* Hot key has been detected *)
                    
                    | WM_HSCROLL of { value    : ScrollDirection,
                                       position : int,
                                       scrollbar   : HWND  }    
                      (* Indicates a click in a horizontal scroll bar *)
                    
                    | WM_HSCROLLCLIPBOARD of { viewer   : HWND,
                                               code     : int,
                                               position : int  }    
                      (* Prompts owner to scroll clipboard contents *)
                    
                    | WM_ICONERASEBKGND of { devicecontext : HDC }
                      (* Notifies minimized Window to fill icon background *)
                    
                    | WM_INITDIALOG of { dialog   : HWND, initdata : int  }
                      (* Initializes a dialog box *)
                    
                    | WM_INITMENU of { menu : HMENU }   
                      (* Indicates a menu is about to become active *)
                    
                    | WM_INITMENUPOPUP of { menupopup  : HMENU,
                                            itemposition : int,
                                            isSystemMenu : bool  }
                      (* Indicates a pop-up menu is being created *)
                    
                    | WM_KEYDOWN of { virtualKey : int, data : KeyData  }   
                      (* Indicates a nonsystem key was pressed *)
                    
                    | WM_KEYUP of { virtualKey : int, data : KeyData  } 
                      (* Indicates a nonsystem key was released *)
                    
                    | WM_KILLFOCUS of { receivefocus : HWND }
                      (* Indicates the Window is losing keyboard focus *)
                    
                    | WM_LBUTTONDBLCLK of { keyflags : MouseKeyFlags list, x : int, y : int  }
                      (* Indicates double-click of left button *) 
                    
                    | WM_LBUTTONDOWN of { keyflags : MouseKeyFlags list, x : int, y : int  }
                      (* Indicates when left mouse button is pressed *)
                    
                    | WM_LBUTTONUP of { keyflags : MouseKeyFlags list, x : int, y : int  }
                      (* Indicates when left mouse button is released *)
                    
                    | WM_MBUTTONDBLCLK of { keyflags : MouseKeyFlags list, x : int, y : int  }
                      (* Indicates double-click of middle mouse button *)
                    
                    | WM_MBUTTONDOWN of { keyflags : MouseKeyFlags list, x : int, y : int  }
                      (* Indicates when middle mouse button is pressed *)
                    
                    | WM_MBUTTONUP of { keyflags : MouseKeyFlags list, x : int, y : int  }
                      (* Indicates when middle mouse button is released *)
                          
                    | WM_MDICASCADE of { skipDisabled : bool  } 
                      (* Arranges MDI child Windows in cascade format *)
                    
                    | WM_MDICREATE of { class : ClassType,
                                         title : string,
                                         instance: HINSTANCE,
                                         x : int,
                                         y : int,
                                         cx : int,
                                         cy : int,
                                         style : int,
                                         cdata : int }  
                      (* Prompts MDI client to create a child Window *) 
                    
                    | WM_MDIDESTROY of { child : HWND  }    
                      (* Closes an MDI child Window *) 
                    
                    | WM_MDIGETACTIVE
                      (* Retrieves data about the active MDI child Window *) 
                    
                    | WM_MDIICONARRANGE 
                      (* Arranges minimized MDI child Windows *) 
                    
                    | WM_MDIMAXIMIZE of {  child: HWND  }   
                      (* Maximizes an MDI child Window *) 
                    
                    | WM_MDINEXT of { child    : HWND, flagnext : bool  }
                      (* Activates the next MDI child Window *) 
                    
                    | WM_MDIREFRESHMENU
                      (* Refreshes an MDI frame Window's menu *) 
                     
                    | WM_MDIRESTORE of {  child : HWND  }
                      (* Prompts MDI client to restore a child Window *) 
                    
                    | WM_MDISETMENU  of { frameMenu  : HMENU, windowMenu : HMENU  } 
                      (* Replaces an MDI frame Window's menu *) 
                    
                    | WM_MDITILE of { tilingflag : MDITileFlags list }
                      (* Arranges MDI child Windows in tiled format *) 
                    
                    | WM_MEASUREITEM of { controlid  : int,
                                          ctlType    : ControlType,
                                          ctlID      : int,
                                          itemID     : int,
                                          itemWidth  : int,
                                          itemHeight : int,
                                          itemData   : int
                                         }  
                      (* Requests dimensions of owner-draw control or item *)

                    | WM_MENUCHAR of { ch : char, menuflag : MenuBase.MenuFlag, menu : HMENU }  
                      (* Indicates an unknown menu mnemonic was pressed *)
                     
                    | WM_MENUSELECT of { menuitem  : int,
                                         menuflags : MenuBase.MenuFlag list,
                                         menu      : HMENU  }
                      (* Indicates that the user selected a menu item *)
                    
                    | WM_MOUSEACTIVATE of { parent   : HWND,
                                            hitTest : int,
                                            message  : int  }   
                      (* Indicates a mouse click in an inactive Window *) 
                    
                    | WM_MOUSEMOVE of { keyflags : MouseKeyFlags list, x : int, y : int  }  
                      (* Indicates mouse-cursor movement *)
                    
                    | WM_MOVE of { x : int, y : int  }  
                      (* Indicates a Window's position has changed *)
                    
                    | WM_NCACTIVATE of { active : bool }
                      (* Changes the active state of nonclient area *)
                    
                    | WM_NCCALCSIZE of { validarea     : bool,
                                         newrect       : RECT ref, (* can be updated. *)
                                         oldrect       : RECT,
                                         oldclientarea : RECT,
                                         hwnd          : HWND,
                                         insertAfter   : HWND,
                                         x     : int,
                                         y     : int,
                                         cx    : int,
                                         cy    : int,
                                         style : WindowPositionStyle list
                                       }
                      (* Calculates the size of a Window's client area *)
                    
                    | WM_NCCREATE of { instance: HINSTANCE,
                                    creation: int,
                                       menu : HMENU,
                                       parent : HWND,
                                       cy : int,
                                       cx : int,
                                       y : int,
                                       x : int,
                                       style : flags,
                                       name : string,
                                       class : ClassType,
                                       extendedstyle : int                      
                                      } 
                      (* Indicates a Window's nonclient area being created *)

                    | WM_NCDESTROY  
                      (* Indicates Window's nonclient area being destroyed *)

                    | WM_NCHITTEST of { x : int, y : int  } 
                      (* Indicates mouse-cursor movement *)
                    
                    | WM_NCLBUTTONDBLCLK of { hitTest : int,
                                              x     : int,
                                              y     : int  }    
                      (* Indicates nonclient left button double-click *)
                     
                    | WM_NCLBUTTONDOWN  of { hitTest : int, x : int, y : int  } 
                      (* Indicates left button pressed in nonclient area *)
                    
                    | WM_NCLBUTTONUP of { hitTest : int, x : int, y : int  }    
                      (* Indicates left button released in nonclient area *)
                    
                    | WM_NCMBUTTONDBLCLK of { hitTest : int, x : int, y : int  }    
                      (* Indicates nonclient middle button double-click *)
                    
                    | WM_NCMBUTTONDOWN of { hitTest : int, x : int, y : int  }  
                      (* Indicates middle button pressed in nonclient area *)
                    
                    | WM_NCMBUTTONUP of { hitTest : int, x : int, y : int  }    
                      (* Indicates middle button released in nonclient area *)
                    
                    | WM_NCMOUSEMOVE of { hitTest : int, x : int, y : int  }    
                      (* Indicates mouse-cursor movement in nonclient area *)
                    
                    | WM_NCPAINT of { region : HRGN  }  
                      (* Indicates a Window's frame needs painting *)
                    
                    | WM_NCRBUTTONDBLCLK of { hitTest : int, x : int, y : int  }    
                      (* Indicates nonclient right button double-click *)
                    
                    | WM_NCRBUTTONDOWN of { hitTest : int, x : int, y : int  }  
                      (* Indicates right button pressed in nonclient area *)
                    
                    | WM_NCRBUTTONUP of { hitTest : int, x : int, y : int  }    
                      (* Indicates right button released in nonclient area *)
                    
                    | WM_NEXTDLGCTL of { control    : int, handleflag : bool  } 
                      (* Sets focus to different dialog box control *) 
                    
                    | WM_PAINT  
                      (* Indicates a Window's client area need painting *)
                    
                    | WM_PAINTCLIPBOARD of { clipboard : HWND }
                      (* Prompts owner to display clipboard contents *)
                    
                    | WM_PAINTICON
                      (* Icon is about to be painted *) 

                    | WM_PALETTECHANGED of { palChg : HWND  }   
                      (* Indicates the focus-Window realized its palette *)
                    
                    | WM_PALETTEISCHANGING of { realize : HWND  }   
                      (* Informs Windows that palette is changing *) 
                    
                    | WM_PARENTNOTIFY of { eventflag : int, idchild   : int, value : int }  
                      (* Notifies parent of child-Window activity *) 
                    
                    | WM_PASTE  
                      (* Inserts clipboard data into an edit control *)
                    
                    | WM_POWER of { powerevent : int  } 
                      (* Indicates the system is entering suspended mode *)
                    
                    | WM_QUERYDRAGICON  
                      (* Requests a cursor handle for a minimized Window *)
                    
                    | WM_QUERYENDSESSION of { source : int  }
                      (* Requests that the Windows session be ended *) 
                    
                    | WM_QUERYNEWPALETTE
                      (* Allows a Window to realize its logical palette *) 
                    
                    | WM_QUERYOPEN
                      (* Requests that a minimized Window be restored *) 
                    
                    | WM_QUEUESYNC
                      (* Delimits CBT messages *) 
                    
                    | WM_QUIT of { exitcode : int  }    
                      (* Requests that an application be terminated *)
                    
                    | WM_RBUTTONDBLCLK of { keyflags : MouseKeyFlags list, x: int, y: int  }    
                      (* Indicates double-click of right mouse button *)
                    
                    | WM_RBUTTONDOWN of { keyflags : MouseKeyFlags list, x: int, y: int  }  
                      (* Indicates when right mouse button is pressed *)

                    | WM_RBUTTONUP of { keyflags : MouseKeyFlags list, x: int, y: int  }
                      (* Indicates when right mouse button is released *) 
                    
                    | WM_RENDERALLFORMATS   
                      (* Notifies owner to render all clipboard formats *) 
                    
                    | WM_RENDERFORMAT of { format : ClipboardFormat  }  
                      (* Notifies owner to render clipboard data *) 
                    
                    | WM_SETCURSOR of { cursorwindow : HWND, hitTest : int, mousemessage : int  }   
                      (* Prompts a Window to set the cursor shape *) 
                    
                    | WM_SETFOCUS of { losing : HWND  }
                      (* Indicates the Window gained the keyboard focus *) 
                    
                    | WM_SETFONT of {font : HFONT, redrawflag : bool  } 
                      (* Sets the font for a control *) 
                    
                    | WM_SETHOTKEY of { virtualKey : int  } 
                      (* Associates a hot key with a Window *) 
                    
                    | WM_SETREDRAW of { redrawflag : int  }
                      (* Allows or prevents redrawing in a Window *) 
                    
                    | WM_SETTEXT of { text : string  }  
                      (* Sets the text of a Window *) 
                    
                    | WM_SHOWWINDOW of { showflag   : bool, statusflag : int  } 
                      (* Indicates a Window is about to be hidden or shown *) 
                    
                    | WM_SIZE of { flag : WMSizeOptions, width : int, height : int  }   
                      (* Indicates a change in a Window's size *)
                    
                    | WM_SIZECLIPBOARD of { viewer : HWND}
                      (* Indicates a change in the chipboard's size *)
                    
                    | WM_SPOOLERSTATUS of { jobstatus : int, jobsleft  : int  } 
                      (* Indicates a print job was added or removed *) 
                    
                    | WM_SYSCHAR of { charCode : char, data : KeyData  }
                      (* Indicates a System-menu key was pressed *)
                    
                    | WM_SYSCOLORCHANGE
                      (* Indicates a system color value was changed *) 
                    
                    | WM_SYSCOMMAND of { commandvalue : SystemCommand, sysBits: int, p: POINT }
                      (* Indicates a system command was requested *) 
                    
                    | WM_SYSDEADCHAR of { charCode : char, data : KeyData  }
                      (* Indicates a system dead key was pressed *) 
                   
                    | WM_SYSKEYDOWN of { virtualKey : int, data : KeyData  }

                    | WM_SYSKEYUP of { virtualKey : int, data : KeyData  }
                      (* Indicates that ALT plus another key was released *)
                    
                    | WM_TIMECHANGE 
                      (* Indicates the system time has been set *)
                    
                    | WM_TIMER of { timerid : int  }
                      (* Indicates timeout interval for a timer has elapsed *)
                    
                    | WM_UNDO   
                      (* Undoes the last operation in an edit control *)
                    
                    | WM_USER of { uMsg: int, wParam: int, lParam: int }
                    | WM_APP of { uMsg: int, wParam: int, lParam: int }
                    | WM_REGISTERED of { uMsg: int, wParam: int, lParam: int }
                    
                    (* Indicates a range of message values *)  
                    (*                  
                      0 through WM_USER - 1 Messages reserved for use by the system. 
                      WM_USER through 0x7FFF Integer messages for use by private window classes. 
                      WM_APP through 0xBFFF Messages available for use by applications. 
                      0xC000 through 0xFFFF String messages for use by applications. 
                      Greater than 0xFFFF Reserved by the system for future use. 
                      WM_USER through 0x7FFF  Integer messages for use by private 
                                              Window classes 
                      0x8000 through 0xBFFF   Messages reserved for future use by Windows 
                      0xC000 through 0xFFFF   String messages for use by applications 
                      Greater than 0xFFFF     Reserved by Windows for future use      *)
                    

                    | WM_VKEYTOITEM of { virtualKey : int,
                                         caretpos   : int,
                                         listbox    : HWND  }
                      (* Provides list-box keystrokes to owner Window *)
                    
                    | WM_VSCROLL of { value     : ScrollDirection,
                                      position  : int,
                                      scrollbar : HWND  }

                      (* Indicates a click in a vertical scroll bar *)
                    
                    | WM_VSCROLLCLIPBOARD of { viewer   : HWND,
                                               code     : int,
                                               position : int  }
                      (* Prompts owner to scroll clipboard contents *) 
                    
                    | WM_WINDOWPOSCHANGED of { hwnd: HWND, front  : HWND,
                                               x   : int,
                                               y   : int,
                                               width  : int,
                                               height : int,
                                               flags  : WindowPositionStyle list }
                      (* Notifies Window of size or position change *)
                    
                    | WM_WINDOWPOSCHANGING of { hwnd: HWND, front: HWND ref,
                                                x   : int ref,
                                                y   : int ref,
                                                width  : int ref,
                                                height : int ref,
                                                flags  : WindowPositionStyle list ref } 
                      (* Notifies Window of new size or position *) 
                    
                    | WM_SETTINGCHANGE of { section_name : string  }    
                      (* Notifies applications that system wide settings have changed *)

                    | WM_NOTIFY of {from: HWND, idCtrl: int, idFrom : int, notification: Notification }

                    | WM_CAPTURECHANGED of { newCapture: HWND }

                    | WM_ENTERSIZEMOVE

                    | WM_EXITSIZEMOVE

                    | WM_PRINT of {hdc: HDC, flags: WMPrintOption list }

                    | WM_PRINTCLIENT of {hdc: HDC, flags: WMPrintOption list }
(*
WM_MOUSEWHEEL                   0x020A
WM_NEXTMENU                     0x0213
WM_SIZING                       0x0214
WM_MOVING                       0x0216
WM_POWERBROADCAST               0x0218
WM_DEVICECHANGE                 0x0219
*)

(*
WM_INPUTLANGCHANGEREQUEST       0x0050
WM_INPUTLANGCHANGE              0x0051
WM_TCARD                        0x0052
WM_USERCHANGED                  0x0054
WM_NOTIFYFORMAT                 0x0055

WM_STYLECHANGING                0x007C
WM_STYLECHANGED                 0x007D
WM_DISPLAYCHANGE                0x007E
*)
                    | WM_HELP of { ctrlId: int, itemHandle: HelpHandle, contextId: int,
                                   mousePos: POINT }

                    | WM_GETICON of { big: bool }

                    | WM_SETICON of { big: bool, icon: HICON }

                    | WM_CONTEXTMENU of { hwnd: HWND, xPos: int, yPos: int }

                    | WM_DISPLAYCHANGE of { bitsPerPixel: int, xScreen: int, yScreen: int }

(* Edit control messages. *)
                    | EM_CANUNDO

                    | EM_CHARFROMPOS of { point: POINT }

                    | EM_EMPTYUNDOBUFFER

                    | EM_FMTLINES of {addEOL: bool}

                    | EM_GETFIRSTVISIBLELINE

(* EM_GETHANDLE - Not implemented.  We can't do anything with it at the moment. *)

                    | EM_GETLIMITTEXT

                    | EM_GETLINE of { lineNo: int, size: int, result: string ref }

                    | EM_GETLINECOUNT

                    | EM_GETMARGINS

                    | EM_GETMODIFY

                    | EM_GETPASSWORDCHAR

                    | EM_GETRECT of {rect: RECT ref}

                    | EM_GETSEL of {startPos: int ref, endPos: int ref}

                    | EM_GETTHUMB

                    | EM_LIMITTEXT of {limit: int}

                    | EM_LINEFROMCHAR of {index: int}

                    | EM_LINEINDEX of {line: int}

                    | EM_LINELENGTH of {index: int}

                    | EM_LINESCROLL of {xScroll: int, yScroll: int}

                    | EM_POSFROMCHAR of {index: int}

                    | EM_REPLACESEL of {canUndo: bool, text: string}

                    | EM_SCROLL of {action: ScrollDirection}

                    | EM_SCROLLCARET

                    | EM_SETMARGINS of {margins: MarginSettings}

                    | EM_SETMODIFY of { modified: bool }

                    | EM_SETPASSWORDCHAR of { ch: char }

                    | EM_SETREADONLY of { readOnly: bool }

                    | EM_SETRECT of {rect: RECT}

                    | EM_SETRECTNP of {rect: RECT}

                    | EM_SETSEL of {startPos: int, endPos: int}

                    | EM_SETTABSTOPS of {tabs: int list}

                    | EM_UNDO

(* Button control messages. *)

                    | BM_CLICK

                    | BM_GETCHECK

                    | BM_GETIMAGE of {imageType: ImageType}

                    | BM_GETSTATE

                    | BM_SETCHECK of {state: int}

                    | BM_SETIMAGE of {image: HGDIOBJ, imageType: ImageType}

                    | BM_SETSTATE of {highlight: bool }

                    | BM_SETSTYLE of {redraw: bool, style: flags}

(* Combobox messages. *)
                    | CB_GETEDITSEL of {startPos: int ref, endPos: int ref}

                    | CB_LIMITTEXT of {limit: int}

                    | CB_SETEDITSEL of {startPos: int, endPos: int}

                    | CB_ADDSTRING of { text: string }

                    | CB_DELETESTRING of { index: int }

                    | CB_GETCOUNT

                    | CB_GETCURSEL

                    | CB_DIR of { attrs: ComboBase.CBDirAttr list, fileSpec: string }

                    | CB_GETLBTEXT of { index: int, length: int, text: string ref }
                        (* Get the text from the list box.  We need to have the length in
                           order to allocate memory for the string although this isn't
                           actually passed in the message. *)

                    | CB_GETLBTEXTLEN of { index: int }

                    | CB_INSERTSTRING of { index: int, text: string }

                    | CB_RESETCONTENT

                    | CB_FINDSTRING of { indexStart: int, text: string }

                    | CB_SELECTSTRING of { indexStart: int, text: string }

                    | CB_SETCURSEL of { index: int }

                    | CB_SHOWDROPDOWN of { show: bool }

                    | CB_GETITEMDATA of { index: int }

                    | CB_SETITEMDATA of { index: int, data: int }

                    | CB_GETDROPPEDCONTROLRECT of { rect: RECT ref }

                    | CB_SETITEMHEIGHT of { index: int, height: int }

                    | CB_GETITEMHEIGHT of { index: int }

                    | CB_SETEXTENDEDUI of { extended: bool }

                    | CB_GETEXTENDEDUI (* Note: Returned value is a bool *)

                    | CB_GETDROPPEDSTATE (* Note: Returned value is a bool *)

                    | CB_FINDSTRINGEXACT of { indexStart: int, text: string }

                    | CB_SETLOCALE of { locale: int } (* Should be an abstract type? *)

                    | CB_GETLOCALE (* Result will be the type used above. *)

                    | CB_GETTOPINDEX

                    | CB_SETTOPINDEX of { index: int }

                    | CB_GETHORIZONTALEXTENT

                    | CB_SETHORIZONTALEXTENT of { extent: int }

                    | CB_GETDROPPEDWIDTH

                    | CB_SETDROPPEDWIDTH of { width: int }

                    | CB_INITSTORAGE of { items: int, bytes: int }

(* Listbox messages *)

                    | LB_ADDSTRING of { text: string }

                    | LB_INSERTSTRING of { index: int, text: string }

                    | LB_DELETESTRING of { index: int }

                    | LB_SELITEMRANGEEX of { first: int, last: int }

                    | LB_RESETCONTENT

                    | LB_SETSEL of { select: bool, index: int }

                    | LB_SETCURSEL of { index: int }

                    | LB_GETSEL of { index: int }

                    | LB_GETCURSEL

                    | LB_GETTEXT of { index: int, length: int, text: string ref }
                        (* Get the text from the list box.  We need to have the length in
                           order to allocate memory for the string although this isn't
                           actually passed in the message. *)

                    | LB_GETTEXTLEN of { index: int }

                    | LB_GETCOUNT

                    | LB_SELECTSTRING of { indexStart: int, text: string }

                    | LB_DIR of { attrs: ComboBase.CBDirAttr list, fileSpec: string }

                    | LB_GETTOPINDEX

                    | LB_FINDSTRING of { indexStart: int, text: string }

                    | LB_GETSELCOUNT

                    | LB_GETSELITEMS of { itemCount: int, items: int list ref }

                    | LB_SETTABSTOPS of { tabs: int list }

                    | LB_GETHORIZONTALEXTENT

                    | LB_SETHORIZONTALEXTENT of { extent: int }

                    | LB_SETCOLUMNWIDTH of { column: int }

                    | LB_ADDFILE of { fileName: string }

                    | LB_SETTOPINDEX of { index: int }

                    | LB_GETITEMRECT of { rect: RECT ref, index: int }

                    | LB_GETITEMDATA of { index: int }

                    | LB_SETITEMDATA of { index: int, data: int }

                    | LB_SELITEMRANGE of { select: bool, first: int, last: int }

                    | LB_SETANCHORINDEX of { index: int }

                    | LB_GETANCHORINDEX

                    | LB_SETCARETINDEX of { index: int, scroll: bool }

                    | LB_GETCARETINDEX

                    | LB_SETITEMHEIGHT of { index: int, height: int }

                    | LB_GETITEMHEIGHT of { index: int }

                    | LB_FINDSTRINGEXACT of { indexStart: int, text: string }

                    | LB_SETLOCALE of { locale: int } (* Should be an abstract type? *)

                    | LB_GETLOCALE (* Result will be the type used above. *)

                    | LB_SETCOUNT of { items: int }

                    | LB_INITSTORAGE of { items: int, bytes: int }

                    | LB_ITEMFROMPOINT of { point: POINT }

(* Static control messages*)

                    | STM_GETICON

                    | STM_GETIMAGE of {imageType: ImageType}

                    | STM_SETICON of {icon: HICON}

                    | STM_SETIMAGE of {image: HGDIOBJ, imageType: ImageType}

(* Scroll bar messages *)
                    | SBM_SETPOS of { pos: int, redraw: bool }

                    | SBM_GETPOS

                    | SBM_SETRANGE of { minPos: int, maxPos: int }

                    | SBM_SETRANGEREDRAW of { minPos: int, maxPos: int }

                    | SBM_GETRANGE of { minPos: int ref, maxPos: int ref }

                    | SBM_ENABLE_ARROWS of ScrollBase.enableArrows

                    | SBM_SETSCROLLINFO of { info: ScrollBase.SCROLLINFO,
                                             options: ScrollBase.ScrollInfoOption list }

                    | SBM_GETSCROLLINFO of { info: ScrollBase.SCROLLINFO ref,
                                             options: ScrollBase.ScrollInfoOption list }

(* Registered messages. *)
                    | FINDMSGSTRING of
                        { flags: FindReplaceFlags.flags, findWhat: string, replaceWith: string }

(* Sent to parent window by a FindReplace dialogue.  The actual argument is a 
   FINDREPLACE structure but this is the only information that may change. *)

                    | NULL


        (* GetMessage and PeekMessage return these values. *)
        type MSG = {
            msg: Message,
            hwnd: HWND,
            time: Time.time,
            pt: {x: int, y: int}
            }

    end
end;
