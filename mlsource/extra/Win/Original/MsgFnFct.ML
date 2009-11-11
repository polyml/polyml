(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

signature importCInterfaceSig = 
sig
    type vol
    type Ctype
    type sym
    type 'a Conversion

    val get_sym  : string -> string -> sym

    val Cchar       : Ctype
    val Cdouble     : Ctype
    val Cfloat      : Ctype
    val Cint        : Ctype
    val Clong       : Ctype
    val Cshort      : Ctype
    val Cpointer    : Ctype -> Ctype
    val Cstruct     : Ctype list -> Ctype
    val Cfunction   : Ctype list * Ctype -> Ctype
    val Cvoid       : Ctype

    val voidStar : Ctype

    val toCstring    : string -> vol
    val toCchar      : string -> vol
    val toCdouble    : real -> vol
    val toCfloat     : real -> vol
    val toCint       : int -> vol
    val toClong      : int -> vol
    val toCshort     : int -> vol

    val fromCstring  : vol -> string
    val fromCchar    : vol -> string
    val fromCdouble  : vol -> real
    val fromCfloat   : vol -> real
    val fromCint     : vol -> int
    val fromClong    : vol -> int
    val fromCshort   : vol -> int

    val break_struct  : Ctype list -> vol -> vol list
    val make_struct   : (Ctype * vol) list -> vol
    
    val alloc        : int -> Ctype -> vol
    val address      : vol -> vol
    val deref        : vol -> vol
    val offset       : int -> Ctype -> vol -> vol
    val assign       : Ctype -> vol -> vol -> unit

    val POINTER : vol Conversion
    val INT     : int Conversion
    val LONG    : int Conversion
    val call1   : sym -> 'a Conversion -> 'b Conversion -> 'a -> 'b

end;

signature importMacroSig = 
sig
  val LOWORD : int -> int
  val HIWORD : int -> int
  val natAnd : int * int -> int 
end;

signature importTypeSig = 
sig
   type vol
   type 'a Conversion
   datatype Id = Id of int
   datatype 'a option = Some of 'a | None 
   datatype MLHWND = MLHWND of vol
   val HWND : MLHWND Conversion
   datatype MLHDC = MLHDC of vol
   datatype MLHMENU = MLHMENU of vol
   datatype MLHANDLE = MLHANDLE of vol
   datatype MLHRGN = MLHRGN of vol
   datatype MLHFONT = MLHFONT of vol
   datatype MLHICON = MLHICON of vol
   datatype MLHCURSOR = MLHCURSOR of vol
   datatype MLHBRUSH = MLHBRUSH of vol
   type Point
   val mkPoint : {x:int,y:int} -> Point 
   type Rectangle
   val mkRect : {left:int,top:int,right:int,bottom:int} -> Rectangle
end;

signature importConstTypeSig = 
sig
   type 'a encoding
   val absE : 'a encoding -> int -> 'a

   type WindowStyleExtended
   type WindowStyle
   type SetWindowPositionStyle
   type SystemCommand
   type HitTest
   type ScrollBarType
   type ControlNotification

   val ScrollBarTypeE : ScrollBarType encoding
   val HitTestE : HitTest encoding
   val WindowStyleExtendedE : WindowStyleExtended encoding
   val WindowStyleE : WindowStyle encoding
   val SetWindowPositionStyleE : SetWindowPositionStyle encoding
   val SystemCommandE : SystemCommand encoding
   val ControlNotificationE : ControlNotification encoding
end;

signature importMsgValSig = 
sig
datatype MessageValue =  MSG_NULL                  (*  0x0000  *)  
                      |  MSG_CREATE                (*  0x0001  *)  
                      |  MSG_DESTROY               (*  0x0002  *)  
                      |  MSG_MOVE                  (*  0x0003  *)  
                      |  MSG_SIZE                  (*  0x0005  *)  
                      |  MSG_ACTIVATE              (*  0x0006  *)  
                      |  MSG_SETFOCUS              (*  0x0007  *)  
                      |  MSG_KILLFOCUS             (*  0x0008  *)  
                      |  MSG_ENABLE                (*  0x000A  *)  
                      |  MSG_SETREDRAW             (*  0x000B  *)  
                      |  MSG_SETTEXT               (*  0x000C  *)  
                      |  MSG_GETTEXT               (*  0x000D  *)  
                      |  MSG_GETTEXTLENGTH         (*  0x000E  *)  
                      |  MSG_PAINT                 (*  0x000F  *)  
                      |  MSG_CLOSE                 (*  0x0010  *)  
                      |  MSG_QUERYENDSESSION       (*  0x0011  *)  
                      |  MSG_QUIT                  (*  0x0012  *)  
                      |  MSG_QUERYOPEN             (*  0x0013  *)  
                      |  MSG_ERASEBKGND            (*  0x0014  *)  
                      |  MSG_SYSCOLORCHANGE        (*  0x0015  *)  
                      |  MSG_ENDSESSION            (*  0x0016  *)  
                      |  MSG_SHOWWINDOW            (*  0x0018  *)  
                      |  MSG_WININICHANGE          (*  0x001A  *)  
                      |  MSG_DEVMODECHANGE         (*  0x001B  *)  
                      |  MSG_ACTIVATEAPP           (*  0x001C  *)  
                      |  MSG_FONTCHANGE            (*  0x001D  *)  
                      |  MSG_TIMECHANGE            (*  0x001E  *)  
                      |  MSG_CANCELMODE            (*  0x001F  *)  
                      |  MSG_SETCURSOR             (*  0x0020  *)  
                      |  MSG_MOUSEACTIVATE         (*  0x0021  *)  
                      |  MSG_CHILDACTIVATE         (*  0x0022  *)  
                      |  MSG_QUEUESYNC             (*  0x0023  *)  
                      |  MSG_GETMINMAXINFO         (*  0x0024  *)  
                      |  MSG_PAINTICON             (*  0x0026  *)  
                      |  MSG_ICONERASEBKGND        (*  0x0027  *)  
                      |  MSG_NEXTDLGCTL            (*  0x0028  *)  
                      |  MSG_SPOOLERSTATUS         (*  0x002A  *)  
                      |  MSG_DRAWITEM              (*  0x002B  *)  
                      |  MSG_MEASUREITEM           (*  0x002C  *)  
                      |  MSG_DELETEITEM            (*  0x002D  *)  
                      |  MSG_VKEYTOITEM            (*  0x002E  *)  
                      |  MSG_CHARTOITEM            (*  0x002F  *)  
                      |  MSG_SETFONT               (*  0x0030  *)  
                      |  MSG_GETFONT               (*  0x0031  *)  
                      |  MSG_SETHOTKEY             (*  0x0032  *)  
                      |  MSG_GETHOTKEY             (*  0x0033  *)  
                      |  MSG_QUERYDRAGICON         (*  0x0037  *)  
                      |  MSG_COMPAREITEM           (*  0x0039  *)  
                      |  MSG_COMPACTING            (*  0x0041  *)  
                      |  MSG_OTHERWINDOWCREATED    (*  0x0042  *)   (* no longer suported *)
                      |  MSG_OTHERWINDOWDESTROYED  (*  0x0043  *)   (* no longer suported *)
                      |  MSG_COMMNOTIFY            (*  0x0044  *)   (* no longer suported *)
                      |  MSG_WINDOWPOSCHANGING     (*  0x0046  *)  
                      |  MSG_WINDOWPOSCHANGED      (*  0x0047  *)  
                      |  MSG_POWER                 (*  0x0048  *)  
                      |  MSG_COPYDATA              (*  0x004A  *)  
                      |  MSG_CANCELJOURNAL         (*  0x004B  *)  
                      |  MSG_NCCREATE              (*  0x0081  *)  
                      |  MSG_NCDESTROY             (*  0x0082  *)  
                      |  MSG_NCCALCSIZE            (*  0x0083  *)  
                      |  MSG_NCHITTEST             (*  0x0084  *)  
                      |  MSG_NCPAINT               (*  0x0085  *)  
                      |  MSG_NCACTIVATE            (*  0x0086  *)  
                      |  MSG_GETDLGCODE            (*  0x0087  *)  
                      |  MSG_NCMOUSEMOVE           (*  0x00A0  *)  
                      |  MSG_NCLBUTTONDOWN         (*  0x00A1  *)  
                      |  MSG_NCLBUTTONUP           (*  0x00A2  *)  
                      |  MSG_NCLBUTTONDBLCLK       (*  0x00A3  *)  
                      |  MSG_NCRBUTTONDOWN         (*  0x00A4  *)  
                      |  MSG_NCRBUTTONUP           (*  0x00A5  *)  
                      |  MSG_NCRBUTTONDBLCLK       (*  0x00A6  *)  
                      |  MSG_NCMBUTTONDOWN         (*  0x00A7  *)  
                      |  MSG_NCMBUTTONUP           (*  0x00A8  *)  
                      |  MSG_NCMBUTTONDBLCLK       (*  0x00A9  *)   
                      |  MSG_KEYDOWN               (*  0x0100  *)  
                      |  MSG_KEYUP                 (*  0x0101  *)  
                      |  MSG_CHAR                  (*  0x0102  *)  
                      |  MSG_DEADCHAR              (*  0x0103  *)  
                      |  MSG_SYSKEYDOWN            (*  0x0104  *)  
                      |  MSG_SYSKEYUP              (*  0x0105  *)  
                      |  MSG_SYSCHAR               (*  0x0106  *)  
                      |  MSG_SYSDEADCHAR           (*  0x0107  *)  
                      |  MSG_KEYLAST               (*  0x0108  *)  
                      |  MSG_INITDIALOG            (*  0x0110  *)  
                      |  MSG_COMMAND               (*  0x0111  *)  
                      |  MSG_SYSCOMMAND            (*  0x0112  *)  
                      |  MSG_TIMER                 (*  0x0113  *)  
                      |  MSG_HSCROLL               (*  0x0114  *)  
                      |  MSG_VSCROLL               (*  0x0115  *)  
                      |  MSG_INITMENU              (*  0x0116  *)  
                      |  MSG_INITMENUPOPUP         (*  0x0117  *)  
                      |  MSG_MENUSELECT            (*  0x011F  *)  
                      |  MSG_MENUCHAR              (*  0x0120  *)  
                      |  MSG_ENTERIDLE             (*  0x0121  *)  
                      |  MSG_CTLCOLORMSGBOX        (*  0x0132  *)  
                      |  MSG_CTLCOLOREDIT          (*  0x0133  *)  
                      |  MSG_CTLCOLORLISTBOX       (*  0x0134  *)  
                      |  MSG_CTLCOLORBTN           (*  0x0135  *)  
                      |  MSG_CTLCOLORDLG           (*  0x0136  *)  
                      |  MSG_CTLCOLORSCROLLBAR     (*  0x0137  *)  
                      |  MSG_CTLCOLORSTATIC        (*  0x0138  *)   
                      |  MSG_MOUSEMOVE             (*  0x0200  *)  
                      |  MSG_LBUTTONDOWN           (*  0x0201  *)  
                      |  MSG_LBUTTONUP             (*  0x0202  *)  
                      |  MSG_LBUTTONDBLCLK         (*  0x0203  *)  
                      |  MSG_RBUTTONDOWN           (*  0x0204  *)  
                      |  MSG_RBUTTONUP             (*  0x0205  *)  
                      |  MSG_RBUTTONDBLCLK         (*  0x0206  *)  
                      |  MSG_MBUTTONDOWN           (*  0x0207  *)  
                      |  MSG_MBUTTONUP             (*  0x0208  *)  
                      |  MSG_MBUTTONDBLCLK         (*  0x0209  *)   
                      |  MSG_PARENTNOTIFY          (*  0x0210  *)  
                      |  MSG_ENTERMENULOOP         (*  0x0211  *)  
                      |  MSG_EXITMENULOOP          (*  0x0212  *)  
                      |  MSG_MDICREATE             (*  0x0220  *)  
                      |  MSG_MDIDESTROY            (*  0x0221  *)  
                      |  MSG_MDIACTIVATE           (*  0x0222  *)  
                      |  MSG_MDIRESTORE            (*  0x0223  *)  
                      |  MSG_MDINEXT               (*  0x0224  *)  
                      |  MSG_MDIMAXIMIZE           (*  0x0225  *)  
                      |  MSG_MDITILE               (*  0x0226  *)  
                      |  MSG_MDICASCADE            (*  0x0227  *)  
                      |  MSG_MDIICONARRANGE        (*  0x0228  *)  
                      |  MSG_MDIGETACTIVE          (*  0x0229  *)  
                      |  MSG_MDISETMENU            (*  0x0230  *)  
                      |  MSG_DROPFILES             (*  0x0233  *)  
                      |  MSG_MDIREFRESHMENU        (*  0x0234  *)  
                      |  MSG_CUT                   (*  0x0300  *)  
                      |  MSG_COPY                  (*  0x0301  *)  
                      |  MSG_PASTE                 (*  0x0302  *)  
                      |  MSG_CLEAR                 (*  0x0303  *)  
                      |  MSG_UNDO                  (*  0x0304  *)  
                      |  MSG_RENDERFORMAT          (*  0x0305  *)  
                      |  MSG_RENDERALLFORMATS      (*  0x0306  *)  
                      |  MSG_DESTROYCLIPBOARD      (*  0x0307  *)  
                      |  MSG_DRAWCLIPBOARD         (*  0x0308  *)  
                      |  MSG_PAINTCLIPBOARD        (*  0x0309  *)  
                      |  MSG_VSCROLLCLIPBOARD      (*  0x030A  *)  
                      |  MSG_SIZECLIPBOARD         (*  0x030B  *)  
                      |  MSG_ASKCBFORMATNAME       (*  0x030C  *)  
                      |  MSG_CHANGECBCHAIN         (*  0x030D  *)  
                      |  MSG_HSCROLLCLIPBOARD      (*  0x030E  *)  
                      |  MSG_QUERYNEWPALETTE       (*  0x030F  *)  
                      |  MSG_PALETTEISCHANGING     (*  0x0310  *)  
                      |  MSG_PALETTECHANGED        (*  0x0311  *)  
                      |  MSG_HOTKEY                (*  0x0312  *)  
                      |  MSG_PENWINFIRST           (*  0x0380  *)  
                      |  MSG_PENWINLAST            (*  0x038F  *)  
                      |  MSG_USER                  (*  0x0400  *)  

 val i2MessageValue : int -> MessageValue

end;


signature importMsgTypeSig =
sig

   type MLHWND
   type MLHDC
   type MLHMENU
   type MLHANDLE
   type MLHRGN
   type MLHFONT
   type Point
   type Rectangle
   type WindowStyleExtended
   type WindowStyle
   type SetWindowPositionStyle
   type SystemCommand
   type ControlNotification

datatype System     = SYS_ACTIVATE of { w : MLHWND, 
                                       active   : int,
                                       minimize : bool }

                      (* Indicates a change in activation state *)
(*------------------------------------------------------------------------------
WM_ACTIVATE
fActive = LOWORD(wParam);           -o- activation flag -o-
fMinimized = (BOOL) HIWORD(wParam); -o- minimized flag  -o-
MLHWND = (MLHWND) lParam;           -o- window handle   -o-

The WM_ACTIVATE message is sent when a window is being activated or deactivated. This message 
is sent first to the window procedure of the top-level window being deactivated and then to the window 
procedure of the top-level window being activated. 
 ------------------------------------------------------------------------------*)

                    | SYS_ACTIVATEAPP of { w : MLHWND, 
                                          active  : bool,
                                          threadid: int  } 

                      (* Notifies applications when a new task activates *)
(*------------------------------------------------------------------------------
WM_ACTIVATEAPP
fActive = (BOOL) wParam;        -o- activation flag   -o-
dwThreadID = (int) lParam:    -o- thread identifier -o-

The WM_ACTIVATEAPP message is sent when a window beinting to a different application than the 
active window is about to be activated. The message is sent to the application whose window is being 
activated and to the application whose window is being deactivated. 
 ------------------------------------------------------------------------------*)

                    | SYS_ASKCBFORMATNAME of { w : MLHWND, 
                                               formatname : string  }   

                      (* Retrieves the name of the clipboard format *)
(*------------------------------------------------------------------------------
WM_ASKCBFORMATNAME
cchName = (int) wParam            -o- size of buffer -o-
lpszFormatName = (LPTSTR) lParam  -o- buffer to receive format name -o-

The WM_ASKCBFORMATNAME message is sent to the clipboard owner by a clipboard viewer window to 
request the name of a CF_OWNERDISPLAY clipboard format. 
 ------------------------------------------------------------------------------*)

                    | SYS_CANCELJOURNAL 

                      (* Notifies application when user cancels journaling *)
(*------------------------------------------------------------------------------
WM_CANCELJOURNAL
wParam = 0; -o- not used, must be zero -o-
lParam = 0; -o- not used, must be zero -o-

The WM_CANCELJOURNAL message is posted to an application when a user cancels the 
application's journaling activities. The message is posted with a NULL window handle. 

 ------------------------------------------------------------------------------*)

                    | SYS_CANCELMODE of { w : MLHWND }  

                      (* Notifies a Window to cancel internal modes *)
(*------------------------------------------------------------------------------
WM_CANCELMODE

The WM_CANCELMODE message is sent to the focus window when a dialog box or message box is 
displayed, enabling the focus window to cancel modes such as mouse capture. 

 ------------------------------------------------------------------------------*)

                    | SYS_CHANGECBCHAIN of { w : MLHWND, 
                                            removed : MLHWND,
                                            next    : MLHWND  } 

                      (* Notifies clipboard viewer of removal from chain *)
(*------------------------------------------------------------------------------
WM_CHANGECBCHAIN
MLHWNDRemove = (MLHWND) wParam;     -o- handle of window being removed -o-
MLHWNDNext = (MLHWND) lParam;       -o- handle of next window in chain -o-

The WM_CHANGECBCHAIN message is sent to the first window in the clipboard viewer chain when a 
window is being removed from the chain. 

 ------------------------------------------------------------------------------*)

                    | SYS_CHAR of { w : MLHWND, 
                                   ch : string,
                                   data : int  }                    

                      (* Indicates the user pressed a character key *)
(*------------------------------------------------------------------------------
WM_CHAR
chCharCode = (TCHAR) wParam;    -o- character code -o-
lKeyData = lParam;              -o- key data       -o-

The WM_CHAR message is posted to the window with the keyboard focus when a WM_KEYDOWN 
message is translated by the TranslateMessage function. WM_CHAR contains the character code of 
the key that was pressed. 

 ------------------------------------------------------------------------------*)

                    | SYS_CHARTOITEM of { w : MLHWND, 
                                         key      : int,
                                         caretpos : int,
                                         listbox  : MLHWND  }

                      (* Provides list-box keystrokes to owner Window *)
(*------------------------------------------------------------------------------
WM_CHARTOITEM
nKey = LOWORD(wParam);          -o- key value          -o-
nCaretPos = HIWORD(wParam);     -o- caret position     -o-
MLHWNDListBox = (MLHWND) lParam;    -o- handle of list box -o-

The WM_CHARTOITEM message is sent by a list box with the LBS_WANTKEYBOARDINPUT style to 
its owner in response to a WM_CHAR message. 

 ------------------------------------------------------------------------------*)

                    | SYS_CHILDACTIVATE of { w : MLHWND }   

                      (* Notifies a child Window of activation *)
(*------------------------------------------------------------------------------
WM_CHILDACTIVATE

The WM_CHILDACTIVATE message is sent to a multiple document interface (MDI) child window when the 
user clicks the window's title bar or when the window is activated, moved, or sized. 

 ------------------------------------------------------------------------------*)

                    | SYS_CHOOSEFONT_GETLOGFONT of { w : MLHWND, 
                                                    lfHeight : int,
                                                    lfWidth : int,
                                                    lfEscapement : int,
                                                    lfOrientation : int,
                                                    lfWeight : int,
                                                    lfItalic : int,
                                                    lfUnderline : int,
                                                    lfStrikeOut : int,
                                                    lfCharSet : int,
                                                    lfOutPrecision : int,
                                                    lfClipPrecision : int,
                                                    lfQuality : int,
                                                    lfPitchAndFamily : int,
                                                    lfFaceName : string
                                                   } 
                                                    
                      (* Retrieves LOGFONT structure for Font dialog box *)
(*------------------------------------------------------------------------------
WM_CHOOSEFONT_GETLOGFONT
wParam = 0;                -o- not used, must be zero            -o-
lParam = (LPLOGFONT) lplf; -o- address of struct. with font data -o-

An application sends a WM_CHOOSEFONT_GETLOGFONT message to the Font dialog box created by 
the ChooseFont function to retrieve the current LOGFONT structure. 

 ------------------------------------------------------------------------------*)

                    | SYS_CLEAR of { w : MLHWND }
                        
                      (* Clears an edit control *)
(*------------------------------------------------------------------------------
WM_CLEAR
wParam = 0; -o- not used, must be zero -o-
lParam = 0; -o- not used, must be zero -o-

An application sends a WM_CLEAR message to an edit control or combo box to delete (clear) the current 
selection, if any, from the edit control. 

 ------------------------------------------------------------------------------*)

                    | SYS_CLOSE of { w : MLHWND }
                        
                      (* System Close menu command was chosen *)
(*------------------------------------------------------------------------------
WM_CLOSE

The WM_CLOSE message is sent as a signal that a window or an application should terminate. 

 ------------------------------------------------------------------------------*)                       

                    | SYS_COMMAND of { w : MLHWND, 
                                      notifycode : ControlNotification,
                                      wid        : int,
                                      control    : MLHWND  }

                      (* Specifies a command message *)
(*------------------------------------------------------------------------------
WM_COMMAND
wNotifyCode = HIWORD(wParam); -o- notification code                        -o-
wID = LOWORD(wParam);         -o- item, control, or accelerator identifier -o-
MLHWNDCtl = (MLHWND) lParam;  -o- handle of control                        -o-

The WM_COMMAND message is sent when the user selects a command item from a menu, when a 
control sends a notification message to its parent window, or when an accelerator keystroke is 
translated. 

 ------------------------------------------------------------------------------*)
                       
                    | SYS_COMPACTING    of { w : MLHWND, 
                                         compactratio : int }

                      (* Indicates a low memory condition *)
(*------------------------------------------------------------------------------
WM_COMPACTING
wCompactRatio = wParam; -o- compacting ratio -o-

The WM_COMPACTING message is sent to all top-level windows when Windows detects more than 12.5 
percent of system time over a 30- to 60-second interval is being spent compacting memory. This indicates 
that system memory is low. 

 ------------------------------------------------------------------------------*)

                    | SYS_COMPAREITEM of { w : MLHWND,
                                           controlid : int,
                                           CtlType : int,
                                           CtlID : int,
                                           hItem : MLHWND,
                                           itemID1 : int,
                                           itemData1 : int,
                                           itemID2 : int,
                                           itemData2 : int                                        
                                           }

                      (* Determines position of combo- or list-box item *)
(*------------------------------------------------------------------------------
WM_COMPAREITEM
idCtl = wParam;                        -o- control identifier   -o-
lpcis = (LPCOMPAREITEMSTRUCT) lParam; -o- structure with items -o-

Windows sends the WM_COMPAREITEM message to determine the relative position of a new item in 
the sorted list of an owner-drawn combo box or list box. Whenever the application adds a new item, 
Windows sends this message to the owner of a combo box or list box created with the CBS_SORT or 
LBS_SORT style. 

 ------------------------------------------------------------------------------*)

                    | SYS_COPY of { w : MLHWND }
                        
                      (* Copies a selection to the clipboard *)
(*------------------------------------------------------------------------------
WM_COPY
wParam = 0; -o- not used, must be zero -o-
lParam = 0; -o- not used, must be zero -o-

An application sends the WM_COPY message to an edit control or combo box to copy the current selection 
to the Clipboard in CF_TEXT format. 

 ------------------------------------------------------------------------------*)

                    | SYS_COPYDATA of { w : MLHWND, 
                                       sender : MLHWND,
                                       data : int }

                      (* Passes data to another application  *)
(*------------------------------------------------------------------------------
WM_COPYDATA
wParam = (WPARAM) (MLHWND) MLHWNDFrom;   -o- handle of sending window -o-
lParam = (LPARAM) (P COPYDATASTRUCT) pcds; -o- pointer to structure with data  -o-

The WM_COPYDATA message is sent when an application passes data to another application. 

 ------------------------------------------------------------------------------*)

                    | SYS_CREATE    of { w : MLHWND,
                                     menu : MLHMENU,
                                     parent : MLHWND,
                                     cy : int,
                                     cx : int,
                                     y : int,
                                     x : int,
                                     style : WindowStyle,
                                     name : string,
                                     classname : string,
                                     extendedstyle : WindowStyleExtended                                                                          
                                     }

                      (* Indicates a Window is being created *)
(*------------------------------------------------------------------------------
WM_CREATE
lpcs = (LP CREATESTRUCT) lParam; -o- structure with creation data -o-

The WM_CREATE message is sent when an application requests that a window be created by calling the 
CreateWindowEx or CreateWindow function. The window procedure of the new window receives this 
message after the window is created, but before the window becomes visible. The message is sent before 
the CreateWindowEx or CreateWindow function returns. 

 ------------------------------------------------------------------------------*)

                    | SYS_CTLCOLORBTN of { w : MLHWND, 
                                          displaycontext : MLHDC,
                                          button         : MLHWND  }

                      (* Button is about to be drawn *)
(*------------------------------------------------------------------------------
WM_CTLCOLORBTN
MLHDCButton = (MLHDC) wParam;   -o- handle of button display context -o-
MLHWNDButton = (MLHWND) lParam; -o- handle of button                 -o-

The WM_CTLCOLORBTN message is sent to the parent window of a button when the button is about to 
be drawn. By responding to this message, the parent window can set a button's text and background 
colors. 

 ------------------------------------------------------------------------------*)

                    | SYS_CTLCOLORDLG of { w : MLHWND, 
                                          displaycontext : MLHDC,
                                          dialogbox      : MLHWND  }

                      (* Dialog box is about to be drawn *)
(*------------------------------------------------------------------------------
WM_CTLCOLORDLG
MLHDCDlg = (MLHDC) wParam;   -o- handle of dialog box display context -o-
MLHWNDDlg = (MLHWND) lParam; -o- handle of dialog box                 -o-

The WM_CTLCOLORDLG message is sent to a dialog box before Windows draws the dialog box. By 
responding to this message, the dialog box can set its text and background colors by using the given 
display device context handle. 

 ------------------------------------------------------------------------------*)
                     
                    | SYS_CTLCOLOREDIT of { w : MLHWND, 
                                           displaycontext : MLHDC,
                                           editcontrol    : MLHWND  }

                      (* Control is about to be drawn *)
(*------------------------------------------------------------------------------
WM_CTLCOLOREDIT
MLHDCEdit = (MLHDC) wParam;   -o- handle of display context -o-
MLHWNDEdit = (MLHWND) lParam; -o- handle of static control  -o-

The WM_CTLCOLOREDIT message is sent to the parent window of an edit control when the control is about 
to be drawn. By responding to this message, the parent window can use the given device context handle to 
set the text and background colors of the edit control. 

 ------------------------------------------------------------------------------*)

                    | SYS_CTLCOLORLISTBOX of { w : MLHWND, 
                                              displaycontext : MLHDC,
                                              listbox        : MLHWND   }
                                                
                      (* List box is about to be drawn *)
(*------------------------------------------------------------------------------
WM_CTLCOLORLISTBOX
MLHDCLB = (MLHDC) wParam;   -o- handle of list box display context -o-
MLHWNDLB = (MLHWND) lParam; -o- handle of list box                 -o-

The WM_CTLCOLORLISTBOX message is sent to the parent window of a list box before Windows draws 
the list box. By responding to this message, the parent window can set the text and background colors of the 
list box by using the given display device context handle. 

 ------------------------------------------------------------------------------*)

                    | SYS_CTLCOLORMSGBOX    of { w : MLHWND, 
                                                 displaycontext : MLHDC,
                                                 messagebox     : MLHWND  }

                      (* Message box is about to be drawn *)
(*------------------------------------------------------------------------------
WM_CTLCOLORMSGBOX
MLHDCMB = (MLHDC) wParam;   -o- handle of message box display context -o-
MLHWNDMB = (MLHWND) lParam; -o- handle of message box                 -o-

The WM_CTLCOLORMSGBOX message is sent to the owner window of a message box before Windows 
draws the message box. By responding to this message, the owner window can set the text and 
background colors of the message box by using the given display device context handle. 

 ------------------------------------------------------------------------------*)

                    | SYS_CTLCOLORSCROLLBAR of { w : MLHWND, 
                                                displaycontext : MLHDC,
                                                scrollbar      : MLHWND  }

                      (* Indicates scroll bar is about to be drawn *)
(*------------------------------------------------------------------------------
WM_CTLCOLORSCROLLBAR
MLHDCSB  = (MLHDC) wParam;   -o- handle of scroll bar display context -o-
MLHWNDSB = (MLHWND) lParam;  -o- handle of scroll bar                 -o-

The WM_CTLCOLORSCROLLBAR message is sent to the parent window of a scroll bar control when the 
control is about to be drawn. By responding to this message, the parent window can use the given display 
context handle to set the background color of the scroll bar control. 

 ------------------------------------------------------------------------------*)

                    | SYS_CTLCOLORSTATIC    of { w : MLHWND, 
                                             displaycontext : MLHDC,
                                             staticcontrol  : MLHWND  }

                      (* Control is about to be drawn *)
(*------------------------------------------------------------------------------
WM_CTLCOLORSTATIC
MLHDCStatic = (MLHDC) wParam;   -o- handle of display context -o-
MLHWNDStatic = (MLHWND) lParam; -o- handle of static control  -o-

The WM_CTLCOLORSTATIC message is sent to the parent window of a static control when the control is 
about to be drawn. By responding to this message, the parent window can use the given device context 
(DC) handle to set the text and background colors of the static control. 

 ------------------------------------------------------------------------------*)

                    | SYS_CUT of { w : MLHWND }
                        
                      (* Deletes a selection and copies it to the clipboard *)
(*------------------------------------------------------------------------------
WM_CUT
wParam = 0; -o- not used, must be zero -o-
lParam = 0; -o- not used, must be zero -o-

An application sends a WM_CUT message to an edit control or combo box to delete (cut) the current 
selection, if any, in the edit control and copy the deleted text to the Clipboard in CF_TEXT format. 

 ------------------------------------------------------------------------------*)

                    | SYS_DDE_ACK
                     
                      (* Acknowledges a DDE message *)
(*------------------------------------------------------------------------------
WM_DDE_ACK
-o- Response to WM_DDE_INITIATE -o-
wParam = (WPARAM) MLHWND;           -o- handle of posting application -o-
lParam = MAKELPARAM(aApp, aTopic) -o- application and topic atoms   -o-

-o- Response to WM_DDE_EXECUTE -o-
wParam = (WPARAM) MLHWND;         -o- handle of posting application       -o-
lParam = (LPARAM) lPackedVal;   -o- packed status flags and data handle -o-

-o- Response to all other messages -o-
wParam = (WPARAM) MLHWND;         -o- handle of posting application -o-
lParam = (LPARAM) lPackedVal;   -o- packed status flags and item  -o-

The WM_DDE_ACK message notifies a dynamic data exchange (DDE) application of the receipt and 
processing of a WM_DDE_INITIATE, WM_DDE_EXECUTE, WM_DDE_DATA, 
WM_DDE_ADVISE, WM_DDE_UNADVISE, or WM_DDE_POKE message, and in some cases, 
of a WM_DDE_REQUEST message. 

 ------------------------------------------------------------------------------*)

                    | SYS_DDE_ADVISE    

                      (* Requests a DDE data-change update *)
(*------------------------------------------------------------------------------
WM_DDE_ADVISE
wParam = (WPARAM) MLHWND;        -o- handle of posting application  -o-
lParam = (LPARAM) lPackedVal;  -o- packed DDEADVISE and item atom -o-

A dynamic data exchange (DDE) client application posts the WM_DDE_ADVISE message to a DDE 
server application to request the server to supply an update for a data item whenever the item changes. 

 ------------------------------------------------------------------------------*)

                    | SYS_DDE_DATA
                        
                      (* Sends data to a DDE client *)
(*------------------------------------------------------------------------------
WM_DDE_DATA
wParam = (WPARAM) MLHWND;         -o- handle of posting application -o-
lParam = (LPARAM) lPackedVal;   -o- packed DDEDATA and item atom  -o-

A dynamic data exchange (DDE) server application posts a WM_DDE_DATA message to a DDE client 
application to pass a data item to the client or to notify the client of the availability of a data item. 

 ------------------------------------------------------------------------------*)
                       
                    | SYS_DDE_EXECUTE
                        
                      (* Sends a string to a DDE server *)
(*------------------------------------------------------------------------------
WM_DDE_EXECUTE
wParam = (WPARAM) MLHWND;        -o- handle of posting application  -o-
lParam = (LPARAM) hCommands;   -o- handle to global object -o-

A dynamic data exchange (DDE) client application posts a WM_DDE_EXECUTE message to a DDE 
server application to send a string to the server to be processed as a series of commands. The server 
application is expected to post a WM_DDE_ACK message in response. 

 ------------------------------------------------------------------------------*)

                    | SYS_DDE_INITIATE  

                      (* Initiates a DDE conversation *)
(*------------------------------------------------------------------------------
WM_DDE_INITIATE
wParam = (WPARAM) MLHWND;             -o- handle of posting appl. -o-
lParam = MAKELPARAM(aApp, aTopic);  -o- appl. and topic atoms   -o-

A dynamic data exchange (DDE) client application sends a WM_DDE_INITIATE message to initiate a 
conversation with a server application responding to the specified application and topic names. Upon 
receiving this message, all server applications with names that match the specified application and that 
support the specified topic are expected to acknowledge it. (For more information, see the 
WM_DDE_ACK message.) 

 ------------------------------------------------------------------------------*)

                    | SYS_DDE_POKE  

                      (* Send unsolicited data to a server *)
(*------------------------------------------------------------------------------
WM_DDE_POKE
wParam = (WPARAM) MLHWND;        -o- handle of posting application  -o-
lParam = (LPARAM) lPackedVal;  -o- packed DDEPOKE and item atom -o-

A dynamic data exchange (DDE) client application posts a WM_DDE_POKE message to a DDE server 
application. A client uses this message to request the server to accept an unsolicited data item. The 
server is expected to reply with a WM_DDE_ACK message indicating whether it accepted the data 
item. 

 ------------------------------------------------------------------------------*)
                       
                    | SYS_DDE_REQUEST   

                      (* Requests data from a DDE server *)
(*------------------------------------------------------------------------------
WM_DDE_REQUEST
wParam = (WPARAM) MLHWND;        -o- handle of posting application  -o-
lParam = (LPARAM) lParam;      -o- holds cfFormat and aItem       -o-

A dynamic data exchange (DDE) client application posts a WM_DDE_REQUEST message to a DDE 
server application to request the value of a data item. 

 ------------------------------------------------------------------------------*)

                    | SYS_DDE_TERMINATE 

                      (* Ends a DDE conversation *)
(*------------------------------------------------------------------------------
WM_DDE_TERMINATE
wParam = (WPARAM) MLHWND; -o- handle of posting window -o-
lParam = 0;             -o- not used, must be zero   -o-


A dynamic data exchange (DDE) application (client or server) posts a WM_DDE_TERMINATE message to 
terminate a conversation. 

 ------------------------------------------------------------------------------*)

                    | SYS_DDE_UNADVISE  

                      (* Stops a DDE data-update request *)
(*------------------------------------------------------------------------------
WM_DDE_UNADVISE
wParam = (WPARAM) MLHWND;        -o- handle of posting application  -o-
lParam = (LPARAM) lParam;      -o- format and item atom           -o-

A dynamic data exchange (DDE) client application posts a WM_DDE_UNADVISE message to inform a 
DDE server application that the specified item or a particular clipboard format for the item should no 
inter be updated. This terminates the warm or hot link for the specified item. 

 ------------------------------------------------------------------------------*)

                    | SYS_DEADCHAR of { w : MLHWND, 
                                       charcode : string,
                                       keydata  : int   }

                      (* Indicates the user pressed a dead key *)
(*------------------------------------------------------------------------------
WM_DEADCHAR
chCharCode = (TCHAR) wParam;    -o- character code -o-
lKeyData = lParam;              -o- key data       -o-

The WM_DEADCHAR message is posted to the window with the keyboard focus when a WM_KEYUP 
message is translated by the TranslateMessage function. WM_DEADCHAR specifies a character 
code generated by a dead key. A dead key is a key that generates a character, such as the umlaut 
(double-dot), that is combined with another character to form a composite character. For example, the 
umlaut-O character (Ö) is generated by typing the dead key for the umlaut character, and then typing the 
O key. 

 ------------------------------------------------------------------------------*)

                    | SYS_DELETEITEM    of { w : MLHWND, 
                                             controlid : int,
                                             CtlType : int,
                                             CtlID : int,
                                             itemID : int,
                                             item : MLHWND,
                                             itemData : int                                         
                                           }

                      (* Indicates owner-draw item or control was altered *)
(*------------------------------------------------------------------------------
WM_DELETEITEM
idCtl = wParam;                      -o- control identifier      -o-
lpdis = (LP DELETEITEMSTRUCT) lParam; -o- structure with item info. -o-

The WM_DELETEITEM message is sent to the owner of an owner-drawn list box or combo box when the list 
box or combo box is destroyed or when items are removed by the LB_DELETESTRING, 
LB_RESETCONTENT, CB_DELETESTRING, or CB_RESETCONTENT message. 

 ------------------------------------------------------------------------------*)

                    | SYS_DESTROY of { w : MLHWND } 

                      (* Indicates Window is about to be destroyed *)
(*------------------------------------------------------------------------------
WM_DESTROY

The WM_DESTROY message is sent when a window is being destroyed. It is sent to the window procedure 
of the window being destroyed after the window is removed from the screen. 
This message is sent first to the window being destroyed and then to the child windows (if any) as they are 
destroyed. During the processing of the message, it can be assumed that all child windows still exist. 

 ------------------------------------------------------------------------------*)

                    | SYS_DESTROYCLIPBOARD of { w : MLHWND }    

                      (* Notifies owner that the clipboard was emptied *)
(*------------------------------------------------------------------------------
WM_DESTROYCLIPBOARD

The WM_DESTROYCLIPBOARD message is sent to the clipboard owner when the clipboard is emptied 
by a call to the EmptyClipboard function. 

 ------------------------------------------------------------------------------*)
                     
                    | SYS_DEVMODECHANGE of { w : MLHWND, 
                                             devicename : string }  

                      (* Indicates the device-mode settings have changed *)
(*------------------------------------------------------------------------------
WM_DEVMODECHANGE
lpszDev = (LPCTSTR) lParam; -o- address of device name -o-

The WM_DEVMODECHANGE message is sent to all top-level windows whenever the user changes 
device-mode settings. 

 ------------------------------------------------------------------------------*)
                     
                    | SYS_DRAWCLIPBOARD of { w : MLHWND }   

                      (* Indicates the clipboard's contents have changed *) 
(*------------------------------------------------------------------------------
WM_DRAWCLIPBOARD

The WM_DRAWCLIPBOARD message is sent to the first window in the clipboard viewer chain when the 
contents of the clipboard change. This enables a clipboard viewer window to display the new contents of 
the clipboard. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_DRAWITEM of { w : MLHWND, 
                                       controlid : int,
                                       CtlType : int,
                                       CtlID : int,
                                       itemID : int,
                                       itemAction : int,
                                       itemState : int,
                                       hItem : MLHWND ,
                                       hDC : MLHDC,
                                       rcItem : Rectangle,
                                       itemData : int
                                        }   

                      (* Indicates owner-draw control/menu needs redrawing *) 
(*------------------------------------------------------------------------------
WM_DRAWITEM
idCtl = (int) wParam;             -o- control identifier       -o-
lpdis = (LP DRAWITEMSTRUCT) lParam; -o- item-drawing information -o-

The WM_DRAWITEM message is sent to the owner window of an owner-drawn button, combo box, list box, 
or menu when a visual aspect of the button, combo box, list box, or menu has changed. 

 ------------------------------------------------------------------------------*)
                     
                    | SYS_DROPFILES of { w : MLHWND,
                                         hDrop : MLHANDLE } 

                      (* Indicates that a file has been dropped *)
(*------------------------------------------------------------------------------
WM_DROPFILES
hDrop = (HANDLE) wParam;  -o- handle of internal drop structure -o-

The WM_DROPFILES message is sent when the user releases the left mouse button while the cursor is in 
the window of an application that has registered itself as a recipient of dropped files. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_ENABLE    of { w : MLHWND, 
                                     enabled : bool }

                      (* Indicates a Window's enable state is changing *)
(*------------------------------------------------------------------------------
WM_ENABLE
fEnabled = (BOOL) wParam;   -o- enabled/disabled flag -o-

The WM_ENABLE message is sent when an application changes the enabled state of a window. It is sent 
to the window whose enabled state is changing. This message is sent before the EnableWindow function 
returns, but after the enabled state (WS_DISABLE style bit) of the window has changed. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_ENDSESSION    of { w : MLHWND, 
                                             endsession : bool }

                      (* Indicates whether the Windows session is ending *)
(*------------------------------------------------------------------------------
WM_ENDSESSION
fEndSession = (BOOL) wParam;    -o- end-session flag -o-

The WM_ENDSESSION message is sent to an application after Windows processes the results of the 
WM_QUERYENDSESSION message. The WM_ENDSESSION message informs the application 
whether the Windows session is ending. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_ENTERIDLE of { w : MLHWND, 
                                        flag   : int,
                                        Window : MLHWND }

                      (* Indicates a modal dialog box or menu is idle *)
(*------------------------------------------------------------------------------
WM_ENTERIDLE
fuSource = wParam;    -o- idle-source flag                     -o-
MLHWND = (MLHWND) lParam; -o- handle of dialog box or owner window -o-


The WM_ENTERIDLE message informs an application's main window procedure that a modal dialog box 
or menu is entering an idle state. A modal dialog box or menu enters an idle state when no messages are 
waiting in its queue after it has processed one or more previous messages. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_ENTERMENULOOP of { w : MLHWND, 
                                            istrack: bool }

                      (* Indicates entry into menu modal loop *)
(*------------------------------------------------------------------------------
WM_ENTERMENULOOP
wParam = (BOOL) fIsTrackPopupMenu    -o- flags a popup menu -o-
lParam = 0 ;          -o- not used; must be zero -o-

The WM_ENTERMENULOOP message informs an application's main window procedure that a menu 
modal loop has been entered. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_EXITMENULOOP of { w : MLHWND, 
                                           istrack: bool }

                      (* Indicates exit from menu modal loop *)
(*------------------------------------------------------------------------------
WM_EXITLOOP
wParam = (BOOL) fIsTrackPopupMenu    -o- flags a popup menu -o-
lParam = 0 ;          -o- not used; must be zero -o-

The WM_ENTERMENULOOP message informs an application's main window procedure that a menu 
modal loop has been exited. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_ERASEBKGND    of { w : MLHWND, 
                                         devicecontext : MLHDC }

                      (* Indicates a Window's background need erasing *)
(*------------------------------------------------------------------------------
WM_ERASEBKGND
MLHDC = (MLHDC) wParam; -o- device-context handle -o-

An application sends the WM_ERASEBKGND message when the window background must be erased (for 
example, when a window is resized). The message is sent to prepare an invalidated portion of a window for 
painting. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_FONTCHANGE    of { w : MLHWND }

                      (* Indicates a change in the font-resource pool *)
(*------------------------------------------------------------------------------
WM_FONTCHANGE
wParam = 0; -o- not used, must be zero -o-
lParam = 0; -o- not used, must be zero -o-

An application sends the WM_FONTCHANGE message to all top-level windows in the system after 
changing the pool of font resources. 

 ------------------------------------------------------------------------------*)
                     
                    | SYS_GETDLGCODE    of { w : MLHWND }

                      (* Allows dialog procedure to process control input *)
(*------------------------------------------------------------------------------
WM_GETDLGCODE

The WM_GETDLGCODE message is sent to the dialog box procedure associated with a control. 
Normally, Windows handles all arrow-key and TAB-key input to the control. By responding to the 
WM_GETDLGCODE message, an application can take control of a particular type of input and process 
the input itself. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_GETFONT of { w : MLHWND } 

                      (* Retrieves the font that a control is using *)
(*------------------------------------------------------------------------------
WM_GETFONT
wParam = 0; -o- not used, must be zero -o-
lParam = 0; -o- not used, must be zero -o-

An application sends a WM_GETFONT message to a control to retrieve the font with which the control is 
currently drawing its text. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_GETHOTKEY of { w : MLHWND }   

                      (* Gets the virtual-key code of a Window's hot key *) 
(*------------------------------------------------------------------------------
WM_GETHOTKEY
wParam = 0; -o- not used, must be zero -o-
lParam = 0; -o- not used, must be zero -o-

An application sends a WM_GETHOTKEY message to determine the hot key associated with a window. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_GETMINMAXINFO of { w : MLHWND,
                                            MaxSizex      : int,
                                            MaxSizey      : int,
                                            MaxPositionx  : int,
                                            MaxPositiony  : int,
                                            MinTrackSizex : int,
                                            MinTrackSizey : int,
                                            MaxTrackSizex : int,
                                            MaxTrackSizey : int                                            
                                             }

                      (* Gets minimum and maximum sizing information *)
(*------------------------------------------------------------------------------
WM_GETMINMAXINFO
lpmmi = (LP MINMAXINFO) lParam; -o- address of structure -o-

The WM_GETMINMAXINFO message is sent to a window when the size or position of the window is about 
to change. An application can use this message to override the window's default maximized size and 
position, or its default minimum or maximum tracking size. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_GETTEXT of { w : MLHWND, 
                                       text : string  } 

                      (* Gets the text that corresponds to a Window *)
(*------------------------------------------------------------------------------
WM_GETTEXT
wParam = (WPARAM) cchTextMax;   -o- number of characters to copy -o-
lParam = (LPARAM) lpszText;     -o- address of buffer for text   -o-

An application sends a WM_GETTEXT message to copy the text that corresponds to a window into a 
buffer provided by the caller. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_GETTEXTLENGTH of { w : MLHWND }   

                      (* Gets length of text associated with a Window *)
(*------------------------------------------------------------------------------
WM_GETTEXTLENGTH
wParam = 0; -o- not used, must be zero -o-
lParam = 0; -o- not used, must be zero -o-

An application sends a WM_GETTEXTLENGTH message to determine the length, in characters, of the text 
associated with a window. The length does not include the terminating null character. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_HOTKEY    of { w : MLHWND, 
                                     id : int }

                      (* Hot key has been detected *)
(*------------------------------------------------------------------------------
WM_HOTKEY
idHotKey = (int) wParam;    -o- identifier of hot-key -o-

The WM_HOTKEY message is posted when the user presses a hot key registered by RegisterHotKey. 
The message is placed at the top of the message queue associated with the thread that registered the hot 
key. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_HSCROLL of { w : MLHWND, 
                                       value    : int,
                                       position : int,
                                       scrollbar   : MLHWND  }  

                      (* Indicates a click in a horizontal scroll bar *)
(*------------------------------------------------------------------------------
WM_HSCROLL
nScrollCode = (int) LOWORD(wParam);   -o- scroll bar value     -o-
nPos = (short int) HIWORD(wParam);   -o- scroll box position  -o-
MLHWNDScrollBar = (MLHWND) lParam;    -o- handle of scroll bar -o-

The WM_HSCROLL message is sent to a window when a scroll event occurs in the window's standard 
horizontal scroll bar. This message is also sent to the owner of a horizontal scroll bar control when a 
scroll event occurs in the control. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_HSCROLLCLIPBOARD of { w : MLHWND, 
                                               viewer   : MLHWND,
                                               code     : int,
                                               position : int  }    

                      (* Prompts owner to scroll clipboard contents *)
(*------------------------------------------------------------------------------
WM_HSCROLLCLIPBOARD
MLHWNDViewer = (MLHWND) wParam;         -o- handle of clipboard viewer -o-
nScrollCode = (int) LOWORD(lParam);     -o- scroll bar code            -o-
nPos = (int) HIWORD(lParam);            -o- scroll box position        -o-

The WM_HSCROLLCLIPBOARD message is sent to the clipboard owner by a clipboard viewer window 
when the clipboard contains data in the CF_OWNERDISPLAY format and an event occurs in the 
clipboard viewer's horizontal scroll bar. The owner should scroll the clipboard image and update the 
scroll bar values. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_ICONERASEBKGND    of { w : MLHWND, 
                                             devicecontext : MLHDC }

                      (* Notifies minimized Window to fill icon background *)
(*------------------------------------------------------------------------------
WM_ICONERASEBKGND
MLHDC = (MLHDC) wParam; -o- handle of device context -o-

The WM_ICONERASEBKGND message is sent to a minimized window when the background of the icon 
must be filled before painting the icon. A window receives this message only if a class icon is defined for the 
window; otherwise, WM_ERASEBKGND is sent. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_INITDIALOG    of { w  : MLHWND, 
                                         dialog   : MLHWND,
                                         initdata : int  
                                         }

                      (* Initializes a dialog box *)
(*------------------------------------------------------------------------------
WM_INITDIALOG
MLHWNDFocus = (MLHWND) wParam; -o- handle of control to receive focus -o-
lInitParam = lParam;       -o- initialization parameter           -o-

The WM_INITDIALOG message is sent to the dialog box procedure immediately before a dialog box is 
displayed. Dialog box procedures typically use this message to initialize controls and carry out any other 
initialization tasks that affect the appearance of the dialog box. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_INITMENU of { w : MLHWND, 
                                        menu : MLHMENU }    

                      (* Indicates a menu is about to become active *)
(*------------------------------------------------------------------------------
WM_INITMENU
hmenuInit = (HMENU) wParam; -o- handle of menu to initialize -o-

The WM_INITMENU message is sent when a menu is about to become active. It occurs when the user 
clicks an item on the menu bar or presses a menu key. This allows the application to modify the menu 
before it is displayed. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_INITMENUPOPUP of { w : MLHWND, 
                                            menupopup  : MLHMENU,
                                            itemposition : int,
                                            issystemmenu : bool  }

                      (* Indicates a pop-up menu is being created *)
(*------------------------------------------------------------------------------
WM_INITMENUPOPUP
hmenuPopup = (HMENU) wParam;         -o- handle of pop-up menu -o-
uPos = (int) LOWORD(lParam);        -o- pop-up item position  -o-
fSystemMenu = (BOOL) HIWORD(lParam); -o- System menu flag      -o-

The WM_INITMENUPOPUP message is sent when a pop-up menu is about to become active. This allows 
an application to modify the pop-up menu before it is displayed, without changing the entire menu. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_KEYDOWN of { w : MLHWND, 
                                      virtualkey : int,
                                      data       : int  }   

                      (* Indicates a nonsystem key was pressed *)
(*------------------------------------------------------------------------------
WM_KEYDOWN
nVirtKey = (int) wParam;    -o- virtual-key code -o-
lKeyData = lParam;          -o- key data         -o-

The WM_KEYDOWN message is posted to the window with the keyboard focus when a nonsystem key 
is pressed. A nonsystem key is a key that is pressed when the ALT key is not pressed. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_KEYUP of { w : MLHWND, 
                                    virtualkey : int,
                                    data       : int  } 

                      (* Indicates a nonsystem key was released *)
(*------------------------------------------------------------------------------
WM_KEYUP
nVirtKey = (int) wParam;    -o- virtual-key code -o-
lKeyData = lParam;          -o- key data         -o-

The WM_KEYUP message is posted to the window with the keyboard focus when a nonsystem key is 
released. A nonsystem key is a key that is pressed when the ALT key is not pressed, or a keyboard key 
that is pressed when a window has the keyboard focus. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_KILLFOCUS of { w : MLHWND, 
                                        receivefocus : MLHWND }

                      (* Indicates the Window is losing keyboard focus *)
(*------------------------------------------------------------------------------
WM_KILLFOCUS
MLHWNDGetFocus = (MLHWND) wParam; -o- handle of window receiving focus -o-

The WM_KILLFOCUS message is sent to a window immediately before it loses the keyboard focus. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_LBUTTONDBLCLK of { w : MLHWND, 
                                            keyflags : int,
                                            xpos     : int,
                                            ypos     : int  }

                      (* Indicates double-click of left button *) 
(*------------------------------------------------------------------------------
WM_LBUTTONDBLCLK
fwKeys = wParam;        -o- key flags                     -o-
xPos = LOWORD(lParam);  -o- horizontal position of cursor -o-
yPos = HIWORD(lParam);  -o- vertical position of cursor   -o-

The WM_LBUTTONDBLCLK message is posted when the user double-clicks the left mouse button while 
the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the 
window beneath the cursor. Otherwise, the message is posted to the window that has captured the 
mouse. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_LBUTTONDOWN of {w : MLHWND, 
                                          keyflags : int,
                                          xpos     : int,
                                          ypos     : int  }

                      (* Indicates when left mouse button is pressed *)
(*------------------------------------------------------------------------------
WM_LBUTTONDOWN
fwKeys = wParam;        -o- key flags                     -o-
xPos = LOWORD(lParam);  -o- horizontal position of cursor -o-
yPos = HIWORD(lParam);  -o- vertical position of cursor   -o-

The WM_LBUTTONDOWN message is posted when the user presses the left mouse button while the 
cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window 
beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_LBUTTONUP of { w : MLHWND, 
                                        keyflags : int,
                                        xpos     : int,
                                        ypos     : int  }

                      (* Indicates when left mouse button is released *)
(*------------------------------------------------------------------------------
WM_LBUTTONUP
fwKeys = wParam;        -o- key flags                     -o-
xPos = LOWORD(lParam);  -o- horizontal position of cursor -o-
yPos = HIWORD(lParam);  -o- vertical position of cursor   -o-

The WM_LBUTTONUP message is posted when the user releases the left mouse button while the 
cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window 
beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MBUTTONDBLCLK of { w : MLHWND, 
                                            keyflags : int,
                                            xpos     : int,
                                            ypos     : int  }

                      (* Indicates double-click of middle mouse button *)
(*------------------------------------------------------------------------------
WM_MBUTTONDBLCLK
fwKeys = wParam;        -o- key flags                     -o-
xPos = LOWORD(lParam);  -o- horizontal position of cursor -o-
yPos = HIWORD(lParam);  -o- vertical position of cursor   -o-

The WM_MBUTTONDBLCLK message is posted when the user double-clicks the middle mouse button 
while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to 
the window beneath the cursor. Otherwise, the message is posted to the window that has captured the 
mouse. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MBUTTONDOWN of { w : MLHWND, 
                                         keyflags : int,
                                         xpos     : int,
                                         ypos     : int  }

                      (* Indicates when middle mouse button is pressed *)
(*------------------------------------------------------------------------------
WM_MBUTTONDOWN
fwKeys = wParam;        -o- key flags                     -o-
xPos = LOWORD(lParam);  -o- horizontal position of cursor -o-
yPos = HIWORD(lParam);  -o- vertical position of cursor   -o-

The WM_MBUTTONDOWN message is posted when the user presses the middle mouse button while 
the cursor is in the client area of a window. If the mouse is not captured, the message is posted to the 
window beneath the cursor. Otherwise, the message is posted to the window that has captured the 
mouse. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MBUTTONUP of { w : MLHWND, 
                                        keyflags : int,
                                        xpos     : int,
                                        ypos     : int  }

                      (* Indicates when middle mouse button is released *)
(*------------------------------------------------------------------------------
WM_MBUTTONUP
fwKeys = wParam;        -o- key flags                     -o-
xPos = LOWORD(lParam);  -o- horizontal position of cursor -o-
yPos = HIWORD(lParam);  -o- vertical position of cursor   -o-

The WM_MBUTTONUP message is posted when the user releases the middle mouse button while the 
cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window 
beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse. 

 ------------------------------------------------------------------------------*)
                          
                    | SYS_MDICASCADE of { w : MLHWND, 
                                         flag : int  }  

                      (* Arranges MDI child Windows in cascade format *)
(*------------------------------------------------------------------------------
WM_MDICASCADE
wParam = (WPARAM) (int) fuCascade;  -o- cascade flag           -o-
lParam = 0;                          -o- not used, must be zero -o-

An application sends the WM_MDICASCADE message to a multiple document interface (MDI) client 
window to arrange all its child windows in a cascade format. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MDICREATE of { w : MLHWND, 
                                         szClass : string,
                                         szTitle : string,
                                         x : int,
                                         y : int,
                                         cx : int,
                                         cy : int,
                                         style : int,
                                         cdata : int }  

                      (* Prompts MDI client to create a child Window *) 
(*------------------------------------------------------------------------------
WM_MDICREATE
wParam = 0;                            -o- not used, must be zero -o-
lParam = (LPARAM) (LP MDICREATESTRUCT) lpmdic;   -o- creation data -o-

An application sends the WM_MDICREATE message to a multiple document interface (MDI) client 
window to create an MDI child window. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MDIDESTROY of { w : MLHWND, 
                                         childtoclose : MLHWND  }   

                      (* Closes an MDI child Window *) 
(*------------------------------------------------------------------------------
WM_MDIDESTROY
wParam = (WPARAM) (MLHWND) MLHWNDChild; -o- handle of child to close   -o-
lParam = 0;                         -o- not used, must be zero     -o-

An application sends the WM_MDIDESTROY message to a multiple document interface (MDI) client 
window to close an MDI child window. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MDIGETACTIVE of { w : MLHWND, 
                                            stateflag : bool  } 

                      (* Retrieves data about the active MDI child Window *) 
(*------------------------------------------------------------------------------
WM_MDIGETACTIVE
wParam = 0;  -o- not used, must be zero -o-
lParam = (LPBOOL) lpfMaximized;  -o- optional pointer to maximized state flag -o-

An application sends the WM_MDIGETACTIVE message to a multiple document interface (MDI) client 
window to retrieve the handle of the active MDI child window. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MDIICONARRANGE of { w : MLHWND }  

                      (* Arranges minimized MDI child Windows *) 
(*------------------------------------------------------------------------------
WM_MDIICONARRANGE
wParam = 0; -o- not used, must be zero -o-
lParam = 0; -o- not used, must be zero -o-

An application sends the WM_MDIICONARRANGE message to a multiple document interface (MDI) client 
window to arrange all minimized MDI child windows. It does not affect child windows that are not minimized. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MDIMAXIMIZE of { w : MLHWND,
                                          childtomaximise : MLHWND  }   

                      (* Maximizes an MDI child Window *) 
(*------------------------------------------------------------------------------
M_MDIMAXIMIZE
wParam = (WPARAM) (MLHWND) MLHWNDMax; -o- handle of child to maximize -o-
lParam = 0;                       -o- not used, must be zero      -o-

An application sends the WM_MDIMAXIMIZE message to a multiple document interface (MDI) client window 
to maximize an MDI child window. Windows resizes the child window to make its client area fill the client 
window. Windows places the child window's System menu icon in the rightmost position of the frame 
window's menu bar, and places the child window's restore icon in the leftmost position. Windows also 
appends the title bar text of the child window to that of the frame window. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MDINEXT of { w : MLHWND, 
                                      child    : MLHWND,
                                      flagnext : bool  }

                      (* Activates the next MDI child Window *) 
(*------------------------------------------------------------------------------
WM_MDINEXT
wParam = (WPARAM) (MLHWND) MLHWNDChild; -o- handle of child        -o-
lParam = (LPARAM) fNext;            -o- next or previous child -o-

An application sends the WM_MDINEXT message to a multiple document interface (MDI) client window to 
activate the next or previous child window. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MDIREFRESHMENU of { w : MLHWND }

                      (* Refreshes an MDI frame Window's menu *) 
(*------------------------------------------------------------------------------
WM_MDIREFRESHMENU
wParam = 0; -o- not used, must be zero -o-
lParam = 0; -o- not used, must be zero -o-

An application sends the WM_MDIREFRESHMENU message to a multiple document interface (MDI) client 
window to refresh the Window menu of the MDI frame window. 

 ------------------------------------------------------------------------------*)
                     
                    | SYS_MDIRESTORE    of { w : MLHWND, 
                                         childtorestore : MLHWND  }

                      (* Prompts MDI client to restore a child Window *) 
(*------------------------------------------------------------------------------
WM_MDIRESTORE
wParam = (WPARAM) (MLHWND) MLHWNDRes; -o- handle of child to restore -o-
lParam = 0;                       -o- not used, must be zero     -o-


An application sends the WM_MDIRESTORE message to a multiple document interface (MDI) client 
window to restore an MDI child window from maximized or minimized size. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MDISETMENU  of { w : MLHWND, 
                                          framemenu  : MLHMENU,
                                          Windowmenu : MLHMENU  } 

                      (* Replaces an MDI frame Window's menu *) 
(*------------------------------------------------------------------------------
WM_MDISETMENU
wParam = (WPARAM) (HMENU) hmenuFrame;  -o- handle of frame menu  -o-
lParam = (LPARAM) (HMENU) hmenuWindow; -o- handle of Window menu -o-

An application sends the WM_MDISETMENU message to a multiple document interface (MDI) client 
window to replace the entire menu of an MDI frame window, to replace the Window menu of the frame 
window, or both. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MDITILE of { w : MLHWND, 
                                      tilingflag : int  }

                      (* Arranges MDI child Windows in tiled format *) 
(*------------------------------------------------------------------------------
WM_MDITILE
wParam = (WPARAM) (int) fuTile; -o- tiling flag            -o-
lParam = 0;                     -o- not used, must be zero -o-

An application sends the WM_MDITILE message to a multiple document interface (MDI) client window to 
arrange all of its MDI child windows in a tile format. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MEASUREITEM of { w : MLHWND, 
                                          controlid  : int,
                                          CtlType    : int,
                                          CtlID      : int,
                                          itemID     : int,
                                          itemWidth  : int,
                                          itemHeight : int,
                                          itemData   : int
                                         }  

                      (* Requests dimensions of owner-draw control or item *)
(*------------------------------------------------------------------------------
WM_MEASUREITEM
idCtl = (int) wParam;                -o- control identifier    -o-
lpmis = (LP MEASUREITEMSTRUCT) lParam; -o- item-size information -o-

The WM_MEASUREITEM message is sent to the owner window of an owner-drawn button, combo box, 
list box, or menu item when the control or menu is created. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MENUCHAR of { w : MLHWND, 
                                       ch       : string,
                                       menuflag : int,
                                       menu     : MLHMENU } 

                      (* Indicates an unknown menu mnemonic was pressed *)
(*------------------------------------------------------------------------------
WM_MENUCHAR
chUser = (char) LOWORD(wParam); -o- ASCII character -o-
fuFlag = (int) HIWORD(wParam);  -o- menu flag       -o-
hmenu = (HMENU) lParam;         -o- handle of menu  -o-

The WM_MENUCHAR message is sent when a menu is active and the user presses a key that does not 
correspond to any mnemonic or accelerator key. This message is sent to the window that owns the 
menu. 

 ------------------------------------------------------------------------------*)
                     
                    | SYS_MENUSELECT    of { w : MLHWND, 
                                         menuitem  : int,
                                         menuflags : int,
                                         menu      : MLHMENU  }

                      (* Indicates that the user selected a menu item *)
(*------------------------------------------------------------------------------
WM_MENUSELECT
uItem = (int) LOWORD(wParam);   -o- menu item or pop-up menu index -o-
fuFlags = (int) HIWORD(wParam); -o- menu flags -o-
hmenu = (HMENU) lParam;          -o- handle of menu clicked on -o-

The WM_MENUSELECT message is sent to a menu's owner window when the user selects a menu 
item. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MOUSEACTIVATE of { w : MLHWND, 
                                            parent   : MLHWND,
                                            hit_test : int,
                                            message  : int  }   

                      (* Indicates a mouse click in an inactive Window *) 
(*------------------------------------------------------------------------------
WM_MOUSEACTIVATE
MLHWNDTopLevel = (MLHWND) wParam;   -o- handle of top-level parent -o-
nHittest = (INT) LOWORD(lParam);    -o- hit-test code              -o-
uMsg =    (int) HIWORD(lParam);     -o- mouse message              -o-

The WM_MOUSEACTIVATE message is sent when the cursor is in an inactive window and the user 
presses a mouse button. The parent window receives this message only if the child window passes it to 
the DefWindowProc function. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MOUSEMOVE of { w : MLHWND, 
                                        keyflags : int,
                                        xpos     : int,
                                        ypos     : int  }   

                      (* Indicates mouse-cursor movement *)
(*------------------------------------------------------------------------------
WM_MOUSEMOVE
fwKeys = wParam;        -o- key flags                     -o-
xPos = LOWORD(lParam);  -o- horizontal position of cursor -o-
yPos = HIWORD(lParam);  -o- vertical position of cursor   -o-

The WM_MOUSEMOVE message is posted to a window when the cursor moves. If the mouse is not 
captured, the message is posted to the window that contains the cursor. Otherwise, the message is 
posted to the window that has captured the mouse. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_MOVE of { w : MLHWND,
                                   xpos : int,
                                   ypos : int  }    

                      (* Indicates a Window's position has changed *)
(*------------------------------------------------------------------------------
WM_MOVE
xPos = (int) LOWORD(lParam);    -o- horizontal position -o-
yPos = (int) HIWORD(lParam);    -o- vertical position   -o-

The WM_MOVE message is sent after a window has been moved. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCACTIVATE of { w : MLHWND, 
                                         active : bool }

                      (* Changes the active state of nonclient area *)
(*------------------------------------------------------------------------------
WM_NCACTIVATE
fActive = (BOOL) wParam;

The WM_NCACTIVATE message is sent to a window when its nonclient area needs to be changed to 
indicate an active or inactive state. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCCALCSIZE    of { w : MLHWND, 
                                             validarea     : bool,
                                             newrect       : Rectangle,
                                             oldrect       : Rectangle,
                                             oldclientarea : Rectangle,
                                             InsertAfter   : MLHWND,
                                             x     : int,
                                             y     : int,
                                             cx    : int,
                                             cy    : int,
                                             flags : int
                                           }

                      (* Calculates the size of a Window's client area *)
(*------------------------------------------------------------------------------
WM_NCCALCSIZE
fCalcValidRects = (BOOL) wParam;        -o- valid area flag -o-
lpncsp = (LP NCCALCSIZE_PARAMS) lParam;  -o- address of data -o-

The WM_NCCALCSIZE message is sent when the size and position of a window's client area must be 
calculated. By processing this message, an application can control the contents of the window's client 
area when the size or position of the window changes. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCCREATE of { w : MLHWND,
                                       menu : MLHMENU,
                                       parent : MLHWND,
                                       cy : int,
                                       cx : int,
                                       y : int,
                                       x : int,
                                       style : int,
                                       name : string,
                                       classname : string,
                                       exstyle : int                      
                                      } 

                      (* Indicates a Window's nonclient area being created *)
(*------------------------------------------------------------------------------
WM_NCCREATE
lpcs = (LP CREATESTRUCT) lParam; -o- initialization data -o-

The WM_NCCREATE message is sent prior to the WM_CREATE message when a window is first 
created. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCDESTROY of { w : MLHWND }   

                      (* Indicates Window's nonclient area being destroyed *)
(*------------------------------------------------------------------------------
WM_NCDESTROY

The WM_NCDESTROY message informs a window that its nonclient area is being destroyed. The 
DestroyWindow function sends the WM_NCDESTROY message to the window following the 
WM_DESTROY message. WM_DESTROY is used to free the allocated memory object associated with 
the window. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCHITTEST of { w : MLHWND, 
                                        xpos : int,
                                        ypos : int  }   

                      (* Indicates mouse-cursor movement *)
(*------------------------------------------------------------------------------
WM_NCHITTEST
xPos = LOWORD(lParam);  -o- horizontal position of cursor -o-
yPos = HIWORD(lParam);  -o- vertical position of cursor   -o-

The WM_NCHITTEST message is sent to a window when the cursor moves, or when a mouse button is 
pressed or released. If the mouse is not captured, the message is sent to the window beneath the cursor. 
Otherwise, the message is posted to the window that has captured the mouse. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCLBUTTONDBLCLK of { w : MLHWND, 
                                              hit_test : int,
                                              xpos     : int,
                                              ypos     : int  } 

                      (* Indicates nonclient left button double-click *)
(*------------------------------------------------------------------------------
WM_NCLBUTTONDBLCLK
nHittest = (INT) wParam;    -o- hit-test code            -o-
pts = MAKEPOINTS(lParam);   -o- position of cursor       -o-

The WM_NCLBUTTONDBLCLK message is posted when the user double-clicks the left mouse button 
while the cursor is within the nonclient area of a window. This message is posted to the window that 
contains the cursor. If a window has captured the mouse, this message is not posted. 

 ------------------------------------------------------------------------------*)
                     
                    | SYS_NCLBUTTONDOWN of { w : MLHWND, 
                                             hit_test : int,
                                             xpos     : int,
                                             ypos     : int  }  

                      (* Indicates left button pressed in nonclient area *)
(*------------------------------------------------------------------------------
WM_NCLBUTTONDOWN
nHittest = (INT) wParam;    -o- hit-test code            -o-
pts = MAKEPOINTS(lParam);   -o- position of cursor       -o-

The WM_NCLBUTTONDOWN message is posted when the user presses the left mouse button while 
the cursor is within the nonclient area of a window. This message is posted to the window that contains 
the cursor. If a window has captured the mouse, this message is not posted. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCLBUTTONUP of { w : MLHWND,
                                          hit_test : int,
                                          xpos     : int,
                                          ypos     : int  } 

                      (* Indicates left button released in nonclient area *)
(*------------------------------------------------------------------------------
WM_NCLBUTTONUP
nHittest = (INT) wParam;    -o- hit-test code            -o-
pts = MAKEPOINTS(lParam);   -o- mouse-cursor coordinates -o-

The WM_NCLBUTTONUP message is posted when the user releases the left mouse button while the 
cursor is within the nonclient area of a window. This message is posted to the window that contains the 
cursor. If a window has captured the mouse, this message is not posted. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCMBUTTONDBLCLK of { w : MLHWND, 
                                              hit_test : int,
                                              xpos     : int,
                                              ypos     : int  } 

                      (* Indicates nonclient middle button double-click *)
(*------------------------------------------------------------------------------
WM_NCMBUTTONDBLCLK
nHittest = (INT) wParam;    -o- hit-test code            -o-
pts = MAKEPOINTS(lParam);   -o- position of cursor       -o-

The WM_NCMBUTTONDBLCLK message is posted when the user double-clicks the middle mouse 
button while the cursor is within the nonclient area of a window. This message is posted to the window that 
contains the cursor. If a window has captured the mouse, this message is not posted. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCMBUTTONDOWN of { w : MLHWND, 
                                            hit_test : int,
                                            xpos     : int,
                                            ypos     : int  }   

                      (* Indicates middle button pressed in nonclient area *)
(*------------------------------------------------------------------------------
WM_NCMBUTTONDOWN
nHittest = (INT) wParam;    -o- hit-test code            -o-
pts = MAKEPOINTS(lParam);   -o- position of cursor       -o-

The WM_NCMBUTTONDOWN message is posted when the user presses the middle mouse button while 
the cursor is within the nonclient area of a window. This message is posted to the window that contains the 
cursor. If a window has captured the mouse, this message is not posted. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCMBUTTONUP of { w : MLHWND, 
                                          hit_test : int,
                                          xpos     : int,
                                          ypos     : int  } 

                      (* Indicates middle button released in nonclient area *)
(*------------------------------------------------------------------------------
WM_NCMBUTTONUP
nHittest = (INT) wParam;    -o- hit-test code            -o-
pts = MAKEPOINTS(lParam);   -o- position of cursor       -o-

The WM_NCMBUTTONUP message is posted when the user releases the middle mouse button while the 
cursor is within the nonclient area of a window. This message is posted to the window that contains the 
cursor. If a window has captured the mouse, this message is not posted. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCMOUSEMOVE of { w : MLHWND, 
                                          hit_test : int,
                                          xpos     : int,
                                          ypos     : int  } 

                      (* Indicates mouse-cursor movement in nonclient area *)
(*------------------------------------------------------------------------------
WM_NCMOUSEMOVE
nHittest = (INT) wParam;    -o- hit-test code            -o-
pts = MAKEPOINTS(lParam);   -o- position of cursor       -o-

The WM_NCMOUSEMOVE message is posted to a window when the cursor is moved within the nonclient 
area of the window. This message is posted to the window that contains the cursor. If a window has captured 
the mouse, this message is not posted. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCPAINT of { w : MLHWND, 
                                      region : MLHRGN  }    

                      (* Indicates a Window's frame needs painting *)
(*------------------------------------------------------------------------------
WM_NCPAINT
hrgn = (HRGN) wParam;   -o- update-region handle -o-

An application sends the WM_NCPAINT message to a window when its frame must be painted. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCRBUTTONDBLCLK of { w : MLHWND, 
                                              hit_test : int,
                                              xpos     : int,
                                              ypos     : int  } 

                      (* Indicates nonclient right button double-click *)
(*------------------------------------------------------------------------------
WM_NCRBUTTONDBLCLK
nHittest = (INT) wParam;    -o- hit-test code            -o-
pts = MAKEPOINTS(lParam);   -o- position of cursor       -o-

The WM_NCRBUTTONDBLCLK message is posted when the user double-clicks the right mouse button 
while the cursor is within the nonclient area of a window. This message is posted to the window that 
contains the cursor. If a window has captured the mouse, this message is not posted. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCRBUTTONDOWN of { w : MLHWND, 
                                            hit_test : int,
                                            xpos     : int,
                                            ypos     : int  }   

                      (* Indicates right button pressed in nonclient area *)
(*------------------------------------------------------------------------------
WM_NCRBUTTONDOWN
nHittest = (INT) wParam;    -o- hit-test code            -o-
pts = MAKEPOINTS(lParam);   -o- position of cursor       -o-

The WM_NCRBUTTONDOWN message is posted when the user presses the right mouse button while the 
cursor is within the nonclient area of a window. This message is posted to the window that contains the 
cursor. If a window has captured the mouse, this message is not posted. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NCRBUTTONUP of { w : MLHWND, 
                                          hit_test : int,
                                          xpos     : int,
                                          ypos     : int  } 

                      (* Indicates right button released in nonclient area *)
(*------------------------------------------------------------------------------
WM_NCRBUTTONUP
nHittest = (INT) wParam;    -o- hit-test code            -o-
pts = MAKEPOINTS(lParam);   -o- position of cursor       -o-

The WM_NCRBUTTONUP message is posted when the user releases the right mouse button while the 
cursor is within the nonclient area of a window. This message is posted to the window that contains the 
cursor. If a window has captured the mouse, this message is not posted. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_NEXTDLGCTL    of { w : MLHWND, 
                                         control    : int,
                                         handleflag : bool  } 

                      (* Sets focus to different dialog box control *) 
(*------------------------------------------------------------------------------
WM_NEXTDLGCTL
wCtlFocus = wParam;              -o- identifies control for focus -o-
fHandle = (BOOL) LOWORD(lParam); -o- wParam handle flag           -o-

The WM_NEXTDLGCTL message is sent to a dialog box procedure to set the keyboard focus to a 
different control in the dialog box. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_PAINT of { w : MLHWND  }  

                      (* Indicates a Window's client area need painting *)
(*------------------------------------------------------------------------------
WM_PAINT

An application sends the WM_PAINT message when Windows or another application makes a request 
to paint a portion of an application's window. The message is sent when the UpdateWindow or 
RedrawWindow function is called, or by the DispatchMessage function when the application obtains 
a WM_PAINT message by using the GetMessage or PeekMessage function. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_PAINTCLIPBOARD of { w : MLHWND, 
                                             clipboard : MLHWND }

                      (* Prompts owner to display clipboard contents *)
(*------------------------------------------------------------------------------
WM_PAINTCLIPBOARD
MLHWNDViewer = (MLHWND) wParam;  -o- handle of clipboard viewer   -o-
hglbPs = (HGLOBAL) lParam;       -o- handle of PAINTSTRUCT object -o-

The WM_PAINTCLIPBOARD message is sent to the clipboard owner by a clipboard viewer window when 
the clipboard contains data in the CF_OWNERDISPLAY format and the clipboard viewer's client area 
needs repainting. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_PAINTICON of { w : MLHWND }

                      (* Icon is about to be painted *) 

(*------------------------------------------------------------------------------
WM_PAINTICON

The WM_PAINTICON message is sent to a minimized window when the icon is to be painted. A window 
receives this message only if a class icon is defined for the window. Otherwise, WM_PAINT is sent 
instead. 

 ------------------------------------------------------------------------------*)                    

                    | SYS_PALETTECHANGED of { w : MLHWND, 
                                             changedpalette : MLHWND  } 

                      (* Indicates the focus-Window realized its palette *)
(*------------------------------------------------------------------------------
WM_PALETTECHANGED
MLHWNDPalChg = (MLHWND) wParam; -o- handle of window that changed palette -o-

The WM_PALETTECHANGED message is sent to all top-level and overlapped windows after the window 
with the keyboard focus has realized its logical palette, thereby changing the system palette. This message 
enables a window without the keyboard focus that uses a color palette to realize its logical palette and 
update its client area. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_PALETTEISCHANGING of { w : MLHWND, 
                                                changingpalette : MLHWND  } 

                      (* Informs Windows that palette is changing *) 
(*------------------------------------------------------------------------------
WM_PALETTEISCHANGING
MLHWNDRealize = (MLHWND) wParam; -o- window to realize palette -o-

The WM_PALETTEISCHANGING message informs applications that an application is going to realize its 
logical palette. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_PARENTNOTIFY of { w : MLHWND, 
                                           eventflag : int,
                                           idchild   : int,
                                           value     : int
                                          } 

                      (* Notifies parent of child-Window activity *) 
(*------------------------------------------------------------------------------
WM_PARENTNOTIFY
fwEvent = LOWORD(wParam);  -o- event flags                         -o-
idChild = HIWORD(wParam);  -o- identifier of child window          -o-
lValue = lParam;           -o- child handle, or cursor coordinates -o-

The WM_PARENTNOTIFY message is sent to the parent of a child window when the child window is 
created or destroyed or when the user clicks a mouse button while the cursor is over the child window. 
When the child window is being created, the system sends WM_PARENTNOTIFY just before the 
CreateWindow or CreateWindowEx function that creates the window returns. When the child window 
is being destroyed, Windows sends the message before any processing to destroy the window takes 
place. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_PASTE of { w : MLHWND }   

                      (* Inserts clipboard data into an edit control *)
(*------------------------------------------------------------------------------
WM_PASTE
wParam = 0; -o- not used, must be zero -o-
lParam = 0; -o- not used, must be zero -o-

An application sends a WM_PASTE message to an edit control or combo box to copy the current contents 
of the Clipboard to the edit control at the current caret position. Data is inserted only if the Clipboard contains 
data in CF_TEXT format. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_POWER of { w : MLHWND, 
                                    powerevent : int  } 

                      (* Indicates the system is entering suspended mode *)
(*------------------------------------------------------------------------------
WM_POWER
fwPowerEvt = wParam;    -o- power-event notification message -o-

The WM_POWER message is sent when the system, typically a battery-powered personal computer, is 
about to enter the suspended mode. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_QUERYDRAGICON of { w : MLHWND }   

                      (* Requests a cursor handle for a minimized Window *)
(*------------------------------------------------------------------------------
WM_QUERYDRAGICON

The WM_QUERYDRAGICON message is sent to a minimized (iconic) window which is about to be 
dragged by the user but which does not have an icon defined for its class. An application can return the 
handle of an icon or cursor. The system displays this cursor or icon while the user drags the icon. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_QUERYENDSESSION of { w : MLHWND, 
                                              source : int  }

                      (* Requests that the Windows session be ended *) 
(*------------------------------------------------------------------------------
WM_QUERYENDSESSION
nSource = (int) wParam;    -o- source of end-session request -o-

The WM_QUERYENDSESSION message is sent when the user chooses to end the Windows session or 
when an application calls the ExitWindows function. If any application returns zero, the Windows session is 
not ended. Windows stops sending WM_QUERYENDSESSION messages as soon as one application 
returns zero. 
After processing this message, Windows sends the WM_ENDSESSION message with the wParam 
parameter set to the results of the WM_QUERYENDSESSION message. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_QUERYNEWPALETTE of { w : MLHWND }

                      (* Allows a Window to realize its logical palette *) 
(*------------------------------------------------------------------------------
 WM_QUERYNEWPALETTE

The WM_QUERYNEWPALETTE message informs a window that it is about to receive the keyboard focus, 
giving the window the opportunity to realize its logical palette when it receives the focus. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_QUERYOPEN of { w : MLHWND }

                      (* Requests that a minimized Window be restored *) 
(*------------------------------------------------------------------------------
 WM_QUERYOPEN

The WM_QUERYOPEN message is sent to an icon when the user requests that the window be restored to 
its previous size and position. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_QUEUESYNC of { w : MLHWND }

                      (* Delimits CBT messages *) 
(*------------------------------------------------------------------------------
WM_QUEUESYNC
wParam = 0; -o- not used, must be zero -o-
lParam = 0; -o- not used, must be zero -o-

The WM_QUEUESYNC message is sent by a computer-based training (CBT) application to separate 
user-input messages from other messages sent through the WH_JOURNALPLAYBACK hook 
procedure. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_QUIT of { w : MLHWND, 
                                   exitcode : int  }    

                      (* Requests that an application be terminated *)
(*------------------------------------------------------------------------------
WM_QUIT
nExitCode = (int) wParam;   -o- exit code -o-

The WM_QUIT message indicates a request to terminate an application and is generated when the 
application calls the PostQuitMessage function. It causes the GetMessage function to return zero. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_RBUTTONDBLCLK of { w : MLHWND, 
                                            keyflags : int,
                                            xpos    : int,
                                            ypos    : int  }    

                      (* Indicates double-click of right mouse button *)
(*------------------------------------------------------------------------------
WM_RBUTTONDBLCLK
fwKeys = wParam;        -o- key flags                     -o-
xPos = LOWORD(lParam);  -o- horizontal position of cursor -o-
yPos = HIWORD(lParam);  -o- vertical position of cursor   -o-

The WM_RBUTTONDBLCLK message is posted when the user double-clicks the right mouse button 
while the cursor is in the client area of a window. If the mouse is not captured, the message is posted to 
the window beneath the cursor. Otherwise, the message is posted to the window that has captured the 
mouse. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_RBUTTONDOWN of { w : MLHWND, 
                                          keyflags : int,
                                          xpos    : int,
                                          ypos    : int  }  

                      (* Indicates when right mouse button is pressed *) 
(*------------------------------------------------------------------------------
WM_RBUTTONDOWN
fwKeys = wParam;        -o- key flags                     -o-
xPos = LOWORD(lParam);  -o- horizontal position of cursor -o-
yPos = HIWORD(lParam);  -o- vertical position of cursor   -o-

The WM_RBUTTONDOWN message is posted when the user presses the right mouse button while the 
cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window 
beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_RBUTTONUP of { w : MLHWND, 
                                        keyflags : int,
                                        xpos    : int,
                                        ypos    : int  }

                      (* Indicates when right mouse button is released *) 
(*------------------------------------------------------------------------------
WM_RBUTTONUP
fwKeys = wParam;        -o- key flags                     -o-
xPos = LOWORD(lParam);  -o- horizontal position of cursor -o-
yPos = HIWORD(lParam);  -o- vertical position of cursor   -o-

The WM_RBUTTONUP message is posted when the user releases the right mouse button while the 
cursor is in the client area of a window. If the mouse is not captured, the message is posted to the window 
beneath the cursor. Otherwise, the message is posted to the window that has captured the mouse. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_RENDERALLFORMATS of { w : MLHWND }    

                      (* Notifies owner to render all clipboard formats *) 
(*------------------------------------------------------------------------------
WM_RENDERALLFORMATS

The WM_RENDERALLFORMATS message is sent to the clipboard owner before it is destroyed, if the 
clipboard owner has delayed rendering one or more clipboard formats. For the contents of the clipboard to 
remain available to other applications, the clipboard owner must render data in all the formats it is capable 
of generating, and place the data on the clipboard by calling the SetClipboardData function. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_RENDERFORMAT of { w : MLHWND, 
                                           format : int  }  

                      (* Notifies owner to render clipboard data *) 
(*------------------------------------------------------------------------------
WM_RENDERFORMAT
uFormat = (int) wParam;    -o- clipboard format -o-

The WM_RENDERFORMAT message is sent to the clipboard owner if it has delayed rendering a specific 
clipboard format, and if an application has requested data in that format. The clipboard owner must render 
data in the specified format and place it on the clipboard by calling the SetClipboardData function. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SETCURSOR of { w : MLHWND, 
                                        cursorwindow : MLHWND,
                                        hittest      : int,
                                        mousemessage : int  }   

                      (* Prompts a Window to set the cursor shape *) 
(*------------------------------------------------------------------------------
WM_SETCURSOR
MLHWND = (MLHWND) wParam;    -o- handle of window with cursor -o-
nHittest = LOWORD(lParam);  -o- hit-test code                -o-
wMouseMsg = HIWORD(lParam); -o- mouse-message identifier     -o-

The WM_SETCURSOR message is sent to a window if the mouse causes the cursor to move within a 
window and mouse input is not captured. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SETFOCUS of { w : MLHWND, 
                                       losing : MLHWND  }

                      (* Indicates the Window gained the keyboard focus *) 
(*------------------------------------------------------------------------------
WM_SETFOCUS
MLHWNDLoseFocus = (MLHWND) wParam; -o- handle of window losing focus -o-

The WM_SETFOCUS message is sent to a window after it has gained the keyboard focus. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SETFONT of { w : MLHWND, 
                                      font       : MLHFONT,
                                      redrawflag : int  }   

                      (* Sets the font for a control *) 
(*------------------------------------------------------------------------------
WM_SETFONT
wParam = (WPARAM) hfont;            -o- handle of font -o-
lParam = MAKELPARAM(fRedraw, 0);    -o- redraw flag    -o-

An application sends a WM_SETFONT message to specify the font that a control is to use when drawing 
text. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SETHOTKEY of { w : MLHWND, 
                                        virtualkey : int  } 

                      (* Associates a hot key with a Window *) 
(*------------------------------------------------------------------------------
WM_SETHOTKEY
wParam = (WPARAM) vkey;     -o- virtual-key code of hot key -o-
lParam = 0;                 -o- not used, must be zero      -o-

An application sends a WM_SETHOTKEY message to a window to associate a hot key with the window. 
When the user presses the hot key, the system activates the window. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SETREDRAW of { w : MLHWND, 
                                        redrawflag : int  }

                      (* Allows or prevents redrawing in a Window *) 
(*------------------------------------------------------------------------------
WM_SETREDRAW
wParam = (WPARAM) fRedraw;  -o- state of redraw flag   -o-
lParam = 0;                 -o- not used, must be zero -o-

An application sends the WM_SETREDRAW message to a window to allow changes in that window to be 
redrawn or to prevent changes in that window from being redrawn. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SETTEXT of { w : MLHWND, 
                                       Windowtext : string  }   

                      (* Sets the text of a Window *) 
(*------------------------------------------------------------------------------
WM_SETTEXT
wParam = 0;                     -o- not used, must be zero        -o-
lParam = (LPARAM)(LPCTSTR)lpsz; -o- address of window-text string -o-

An application sends a WM_SETTEXT message to set the text of a window. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SHOWWINDOW    of { w : MLHWND, 
                                             showflag   : bool,
                                             statusflag : int  } 

                      (* Indicates a Window is about to be hidden or shown *) 
(*------------------------------------------------------------------------------
WM_SHOWWINDOW
fShow = (BOOL) wParam;      -o- show/hide flag -o-
fnStatus = (int) lParam;    -o- status flag    -o-

The WM_SHOWWINDOW message is sent to a window when the window is about to be hidden or 
shown. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SIZE of { w : MLHWND, 
                                   resizeflag : int,
                                   width      : int,
                                   height     : int  }  

                      (* Indicates a change in a Window's size *)
(*------------------------------------------------------------------------------
WM_SIZE
fwSizeType = wParam;      -o- resizing flag                     -o-
nWidth = LOWORD(lParam);  -o- width of client area              -o-
nHeight = HIWORD(lParam); -o- height of client area             -o-

The WM_SIZE message is sent to a window after its size has changed. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SIZECLIPBOARD of { w : MLHWND, 
                                            viewer : MLHWND}

                      (* Indicates a change in the chipboard's size *)
(*------------------------------------------------------------------------------
WM_SIZECLIPBOARD
MLHWNDViewer = (MLHWND) wParam;  -o- handle of clipboard viewer -o-
hglbRc = (HGLOBAL) lParam;   -o- handle of RECT object      -o-

The WM_SIZECLIPBOARD message is sent to the clipboard owner by a clipboard viewer window when 
the clipboard contains data in the CF_OWNERDISPLAY format and the clipboard viewer's client area 
has changed size. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SPOOLERSTATUS of { w : MLHWND, 
                                            jobstatus : int,
                                            jobsleft  : int  }  

                      (* Indicates a print job was added or removed *) 
(*------------------------------------------------------------------------------
WM_SPOOLERSTATUS
fwJobStatus = wParam;       -o- job-status flag          -o-
cJobsLeft = LOWORD(lParam); -o- number of jobs remaining -o-

The WM_SPOOLERSTATUS message is sent from Windows Print Manager whenever a job is added to or 
removed from the Print Manager queue. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SYSCHAR of { w : MLHWND, 
                                      ch      : string,
                                      keydata : int  }

                      (* Indicates a System-menu key was pressed *)
(*------------------------------------------------------------------------------
WM_SYSCHAR
chCharCode = (TCHAR) wParam;   -o- character code -o-
lKeyData = lParam;             -o- key data       -o-

The WM_SYSCHAR message is posted to the window with the keyboard focus when a 
WM_SYSKEYDOWN message is translated by the TranslateMessage function. It specifies the 
character code of a system character key--that is, a character key that is pressed while the ALT key is 
down. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SYSCOLORCHANGE of { w : MLHWND }

                      (* Indicates a system color value was changed *) 
(*------------------------------------------------------------------------------
WM_SYSCOLORCHANGE

The WM_SYSCOLORCHANGE message is sent to all top-level windows when a change is made to a 
system color setting. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SYSCOMMAND of { w : MLHWND, 
                                         commandvalue : SystemCommand,
                                         p            : Point }

                      (* Indicates a system command was requested *) 
(*------------------------------------------------------------------------------
WM_SYSCOMMAND
uCmdType = wParam;      -o- command value                 -o-
xPos = LOWORD(lParam);  -o- horizontal position of cursor -o-
yPos = HIWORD(lParam);  -o- vertical position of cursor   -o-

The WM_SYSCOMMAND message is sent when the user chooses a command from the System menu 
(also known as Control menu) or when the user chooses the Maximize button or Minimize button. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SYSDEADCHAR of { w : MLHWND, 
                                          ch      : string,
                                          keydata : int  }

                      (* Indicates a system dead key was pressed *) 
(*------------------------------------------------------------------------------
WM_SYSDEADCHAR
chCharCode = (TCHAR) wParam;    -o- character code -o-
lKeyData = lParam;              -o- key data       -o-

The WM_SYSDEADCHAR message is sent to the window with the keyboard focus when a 
WM_SYSKEYDOWN message is translated by the TranslateMessage function. 
WM_SYSDEADCHAR specifies the character code of a system dead key--that is, a dead key that is 
pressed while holding down the ALT key. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_SYSKEYDOWN of { w : MLHWND, 
                                          virtualkey : int,  
                                          keydata    : int  }


                    | SYS_SYSKEYUP of { w : MLHWND, 
                                       virtualkey : int,  
                                       keydata    : int  }

                      (* Indicates that ALT plus another key was released *)
(*------------------------------------------------------------------------------
WM_SYSKEYDOWN  UP
nVirtKey = (int) wParam;    -o- virtual-key code -o-
lKeyData = lParam;          -o- key data         -o-

The WM_SYSKEYUP message is posted to the window with the keyboard focus when the user releases 
a key that was pressed while the ALT key was held down. It also occurs when no window currently has the 
keyboard focus; in this case, the WM_SYSKEYUP message is sent to the active window. The window 
that receives the message can distinguish between these two contexts by checking the context code in 
the lKeyData parameter. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_TIMECHANGE of { w : MLHWND }  

                      (* Indicates the system time has been set *)
(*------------------------------------------------------------------------------
WM_TIMECHANGE
wParam = 0; -o- not used, must be zero -o-
lParam = 0; -o- not used, must be zero -o-

An application sends the WM_TIMECHANGE message to all top-level windows after changing the system 
time. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_TIMER of { w : MLHWND, 
                                                  timerid : int  }

                      (* Indicates timeout interval for a timer has elapsed *)
(*------------------------------------------------------------------------------
WM_TIMER
wTimerID = wParam;            -o- timer identifier          -o-
tmprc = (TIMERPROC ) lParam; -o- address of timer callback -o-

The WM_TIMER message is posted to the installing thread's message queue or sent to the appropriate 
TimerProc callback function after each interval specified in the SetTimer function used to install a timer. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_UNDO of { w : MLHWND }    

                      (* Undoes the last operation in an edit control *)
(*------------------------------------------------------------------------------
WM_UNDO

An application sends a WM_UNDO message to an edit control to undo the last operation. When this 
message is sent to an edit control, the previously deleted text is restored or the previously added text is 
deleted. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_USER of { w : MLHWND }
                    

                    (* Indicates a range of message values
(*------------------------------------------------------------------------------

 ------------------------------------------------------------------------------*)                    
                      0 through WM_USER - 1   Messages reserved for use by Windows 
                      WM_USER through 0x7FFF  Integer messages for use by private 
                                              Window classes 
                      0x8000 through 0xBFFF   Messages reserved for future use by Windows 
                      0xC000 through 0xFFFF   String messages for use by applications 
                      Greater than 0xFFFF     Reserved by Windows for future use      *)
                    

                    | SYS_VKEYTOITEM    of { w : MLHWND, 
                                         virtualkey : int,
                                         caretpos   : int,
                                         listbox    : MLHWND  }

                      (* Provides list-box keystrokes to owner Window *)
(*------------------------------------------------------------------------------
WM_VKEYTOITEM
vkey = LOWORD(wParam);      -o- virtual-key code   -o-  
nCaretPos = HIWORD(wParam); -o- caret position     -o-
MLHWNDLB = lParam;          -o- handle of list box -o-

The WM_VKEYTOITEM message is sent by a list box with the LBS_WANTKEYBOARDINPUT style to 
its owner in response to a WM_KEYDOWN message. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_VSCROLL of { w : MLHWND, 
                                      value     : int,
                                      position  : int,
                                      scrollbar : MLHWND  }

                      (* Indicates a click in a vertical scroll bar *)
(*------------------------------------------------------------------------------
WM_VSCROLL
nScrollCode = (int) LOWORD(wParam); -o- scroll bar value     -o-
nPos = (short int) HIWORD(wParam);  -o- scroll box position  -o-
MLHWNDScrollBar = (MLHWND) lParam;      -o- handle of scroll bar -o-

The WM_VSCROLL message is sent to a window when a scroll event occurs in the window's standard 
vertical scroll bar. This message is also sent to the owner of a vertical scroll bar control when a scroll 
event occurs in the control. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_VSCROLLCLIPBOARD of { w : MLHWND, 
                                               viewer   : MLHWND,
                                               code     : int,
                                               position : int  }

                      (* Prompts owner to scroll clipboard contents *) 
(*------------------------------------------------------------------------------
WM_VSCROLLCLIPBOARD
MLHWNDViewer = (MLHWND) wParam;         -o- handle of clipboard viewer -o-
nScrollCode = (int) LOWORD(lParam); -o- scroll bar code            -o-
nPos = (int) HIWORD(lParam);        -o- scroll box position        -o-

The WM_VSCROLLCLIPBOARD message is sent to the clipboard owner by a clipboard viewer window 
when the clipboard contains data in the CF_OWNERDISPLAY format and an event occurs in the 
clipboard viewer's vertical scroll bar. The owner should scroll the clipboard image and update the scroll 
bar values. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_WINDOWPOSCHANGED of { w : MLHWND, 
                                               front  : MLHWND,
                                               xpos   : int,
                                               ypos   : int,
                                               width  : int,
                                               height : int,
                                               flags  : SetWindowPositionStyle  }

                      (* Notifies Window of size or position change *)
(*------------------------------------------------------------------------------
WM_WINDOWPOSCHANGED
lpwp = (LP WINDOWPOS) lParam; -o- points to size and position data -o-

The WM_WINDOWPOSCHANGED message is sent to a window whose size, position, or place in the Z 
order has changed as a result of a call to SetWindowPos or another window-management function. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_WINDOWPOSCHANGING of { w : MLHWND,    
                                                front  : MLHWND,
                                                xpos   : int,
                                                ypos   : int,
                                                width  : int,
                                                height : int,
                                                flags  : SetWindowPositionStyle  }  

                      (* Notifies Window of new size or position *) 
(*------------------------------------------------------------------------------
WM_WINDOWPOSCHANGING
lpwp = (LP WINDOWPOS) lParam; -o- points to size and position data -o-

The WM_WINDOWPOSCHANGING message is sent to a window whose size, position, or place in the Z 
order is about to change as a result of a call to SetWindowPos or another window-management 
function. 

 ------------------------------------------------------------------------------*)
                    
                    | SYS_WININICHANGE of { w : MLHWND,
                                           section_name : string  } 

                      (* Notifies applications that WIN.INI has changed *)
(*------------------------------------------------------------------------------
WM_WININICHANGE
wParam = 0;                            -o- not used, must be zero -o-
lParam = (LPARAM) (LPCTSTR) pszSection; -o- section-name string -o-

An application sends the WM_WININICHANGE message to all top-level windows after making a change 
to the WIN.INI file. The SystemParametersInfo function sends the WM_WININICHANGE message 
after an application uses the function to change a setting in WIN.INI. 

 ------------------------------------------------------------------------------*)

                    | SYS_UNKNOWN of { w : MLHWND } 
(*------------------------------------------------------------------------------
  UNKNOWN MESSAGE
 ------------------------------------------------------------------------------*)                    

                    | SYS_NULL;

(*------------------------------------------------------------------------------
  NO MESSAGE
 ------------------------------------------------------------------------------*)

end;

signature importDirSig =
sig
   val wpolyname   : string 
   val gdilibname  : string
   val userlibname : string
end;

functor MsgFnFct( structure Dir : importDirSig
                  structure CInterface : importCInterfaceSig
                  structure Macro : importMacroSig
                  structure Type : importTypeSig
                  structure ConstType : importConstTypeSig
                  structure MsgVal : importMsgValSig
                  structure MsgType : importMsgTypeSig

                  sharing type CInterface.Conversion = 
                               Type.Conversion
                  sharing type CInterface.vol = 
                               Type.vol
                  sharing type Type.MLHMENU =
                               MsgType.MLHMENU
                  sharing type Type.MLHWND = 
                               MsgType.MLHWND
                  sharing type Type.MLHDC = 
                               MsgType.MLHDC
                  sharing type Type.MLHMENU = 
                               MsgType.MLHMENU
                  sharing type Type.MLHANDLE = 
                               MsgType.MLHANDLE
                  sharing type Type.MLHRGN = 
                               MsgType.MLHRGN
                  sharing type Type.MLHFONT = 
                               MsgType.MLHFONT

                  sharing type ConstType.WindowStyleExtended =
                               MsgType.WindowStyleExtended
                  sharing type ConstType.WindowStyle =
                               MsgType.WindowStyle
                  sharing type ConstType.SetWindowPositionStyle =
                               MsgType.SetWindowPositionStyle
                  sharing type ConstType.SystemCommand =
                               MsgType.SystemCommand

                  sharing type Type.Point =
                               MsgType.Point
                  sharing type Type.Rectangle =
                               MsgType.Rectangle
                  sharing type ConstType.ControlNotification =
                               MsgType.ControlNotification
                ) =
struct

open CInterface
open Macro
open Type
open ConstType
open MsgVal
open MsgType
open Dir

local

fun itob i = if i=0 then false else true
val getrts = get_sym wpolyname

fun viewmessage (m,b) = 
if b then TextIO.print("event = "^(Int.toString m)^"\n")
else ()

exception WrongNumberOfVolatiles

fun break_struct3 types v =
let val vols = break_struct types v
in case vols of
   [v1,v2,v3] => 
      (v1,v2,v3)
 | _       =>  raise WrongNumberOfVolatiles   
end;

fun break_struct5 types v =
let val vols = break_struct types v
in case vols of
   [v1,v2,v3,v4,v5] => 
      (v1,v2,v3,v4,v5)
 | _       =>  raise WrongNumberOfVolatiles   
end;

fun break_struct6 types v =
let val vols = break_struct types v
in case vols of
   [v1,v2,v3,v4,v5,v6] => 
      (v1,v2,v3,v4,v5,v6)
 | _       =>  raise WrongNumberOfVolatiles   
end;

fun break_struct7 types v =
let val vols = break_struct types v
in case vols of
   [v1,v2,v3,v4,v5,v6,v7] => 
      (v1,v2,v3,v4,v5,v6,v7)
 | _       =>  raise WrongNumberOfVolatiles   
end;

fun break_struct9 types v =
let val vols = break_struct types v
in case vols of
   [v1,v2,v3,v4,v5,v6,v7,v8,v9] => 
      (v1,v2,v3,v4,v5,v6,v7,v8,v9)
 | _       =>  raise WrongNumberOfVolatiles   
end;

fun break_struct10 types v =
let val vols = break_struct types v
in case vols of
   [v1,v2,v3,v4,v5,v6,v7,v8,v9,v10] => 
      (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10)
 | _       =>  raise WrongNumberOfVolatiles   
end;

fun break_struct12 types v =
let val vols = break_struct types v
in case vols of
   [v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12] => 
      (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12)
 | _       =>  raise WrongNumberOfVolatiles   
end;


in

fun MakeSystem (h,m,wp,lp) =
let val u = viewmessage (m,true)
in
case (i2MessageValue m)  of  

     MSG_NULL           =>  SYS_NULL (* "0x0000" *)

  |  MSG_CREATE         =>  let
                             val (cp,inst,menu,parent,
                                  cy,cx,y,x,
                                  style,
                                  name,classname,
                                  extendedstyle) = break_struct12 ([voidStar,voidStar,voidStar,voidStar,
                                                                  Cint,Cint,Cint,Cint,
                                                                  Clong,
                                                                  Cpointer Cchar,
                                                                  Cpointer Cchar,
                                                                  Clong])
                                                                (deref (toClong lp))
                            in
                            SYS_CREATE { w = h,
                                         menu = MLHMENU menu,
                                         parent = MLHWND parent,
                                         cy = fromCint cy,
                                         cx = fromCint cx,
                                         y = fromCint y,
                                         x = fromCint x,
                                         style = absE 
                                                 WindowStyleE
                                                 (fromClong style),
                                         name = fromCstring name,
                                         classname = fromCstring classname,
                                         extendedstyle = absE 
                                                         WindowStyleExtendedE 
                                                         (fromClong extendedstyle)  
                                        } (* "0x0001" *)
                            end

  |  MSG_DESTROY        =>  SYS_DESTROY { w = h } (* "0x0002" *)

  |  MSG_MOVE           =>  SYS_MOVE { w = h, 
                                       xpos    = LOWORD lp,
                                       ypos    = HIWORD lp } (* "0x0003" *)

  |  MSG_SIZE           =>  SYS_SIZE { w = h, 
                                       resizeflag = wp,
                                       width      = LOWORD lp,
                                       height     = HIWORD lp } (* "0x0005" *)

  |  MSG_ACTIVATE       =>  SYS_ACTIVATE { w = h, 
                                           active = LOWORD wp,
                                           minimize = itob (HIWORD wp) } (* "0x0006" *)

  |  MSG_SETFOCUS       =>  SYS_SETFOCUS { w = h, 
                                           losing = MLHWND (toClong wp) } (* "0x0007" *)

  |  MSG_KILLFOCUS      =>  SYS_KILLFOCUS { w = h, 
                                            receivefocus = MLHWND (toClong wp) } (* "0x0008" *)

  |  MSG_ENABLE         =>  SYS_ENABLE { w = h, 
                                         enabled = itob wp } (* "0x000A" *)

  |  MSG_SETREDRAW      =>  SYS_SETREDRAW { w = h, 
                                            redrawflag = wp  } (* "0x000B" *)

  |  MSG_SETTEXT        =>  SYS_SETTEXT { w = h, 
                                          Windowtext = fromCstring (toClong lp)  } (* "0x000C" *)

  |  MSG_GETTEXT        =>  SYS_GETTEXT { w = h, 
                                          text = fromCstring (toClong lp)  } (* "0x000D" *)

  |  MSG_GETTEXTLENGTH  =>  SYS_GETTEXTLENGTH { w = h } (* "0x000E" *)

  |  MSG_PAINT          =>  SYS_PAINT { w = h  } (* "0x000F" *)

  |  MSG_CLOSE          =>  SYS_CLOSE { w = h } (* "0x0010" *)

  |  MSG_QUERYENDSESSION=>  SYS_QUERYENDSESSION { w = h, 
                                                  source = wp  } (* "0x0011" *)

  |  MSG_QUIT           =>  SYS_QUIT { w  = h, 
                                       exitcode = wp  } (* "0x0012" *)

  |  MSG_QUERYOPEN      =>  SYS_QUERYOPEN { w = h } (* "0x0013" *)

  |  MSG_ERASEBKGND     =>  SYS_ERASEBKGND { w = h, 
                                             devicecontext = MLHDC (toClong wp) } (* "0x0014" *)

  |  MSG_SYSCOLORCHANGE =>  SYS_SYSCOLORCHANGE { w = h } (* "0x0015" *)

  |  MSG_ENDSESSION     =>  SYS_ENDSESSION { w = h, 
                                             endsession = itob wp }(* "0x0016" *)

  |  MSG_SHOWWINDOW     =>  SYS_SHOWWINDOW  { w = h, 
                                              showflag   = itob wp,
                                              statusflag = lp  } (* "0x0018" *)

  |  MSG_WININICHANGE   =>  SYS_WININICHANGE { w = h, 
                                               section_name = fromCstring (toClong lp)  } (* "0x001A" *)

  |  MSG_DEVMODECHANGE  =>  SYS_DEVMODECHANGE { w = h,                         
                                                devicename = fromCstring (toClong lp) } (* "0x001B" *)

  |  MSG_ACTIVATEAPP    =>  SYS_ACTIVATEAPP { w  = h, 
                                              active   = itob wp,
                                              threadid = lp } (* "0x001C" *)

  |  MSG_FONTCHANGE     =>  SYS_FONTCHANGE { w = h } (* "0x001D" *)

  |  MSG_TIMECHANGE     =>  SYS_TIMECHANGE { w = h } (* "0x001E" *)

  |  MSG_CANCELMODE     =>  SYS_CANCELMODE { w = h } (* "0x001F" *)

  |  MSG_SETCURSOR      =>  SYS_SETCURSOR { w = h, 
                                            cursorwindow = MLHWND (toClong wp),
                                            hittest      = LOWORD lp,
                                            mousemessage = HIWORD lp  } (* "0x0020" *)

  |  MSG_MOUSEACTIVATE  =>  SYS_MOUSEACTIVATE { w = h, 
                                                parent   = MLHWND (toClong wp),
                                                hit_test = LOWORD lp,
                                                message  = HIWORD lp  }(* "0x0021" *)

  |  MSG_CHILDACTIVATE  =>  SYS_CHILDACTIVATE { w = h } (* "0x0022" *)

  |  MSG_QUEUESYNC      =>  SYS_QUEUESYNC { w = h } (* "0x0023" *)

  |  MSG_GETMINMAXINFO  =>  let  
                              val (ptresx,ptresy,
                                   ptmsx,ptmsy,
                                   ptmpx,ptmpy,
                                   pttsx,pttsy,
                                   ptmtsx,ptmtsy) = break_struct10 ([Clong,Clong,
                                                                   Clong,Clong,
                                                                   Clong,Clong,
                                                                   Clong,Clong,
                                                                   Clong,Clong])
                                                                 (deref (toClong lp))
                            in
                             SYS_GETMINMAXINFO { w = h, 
                                                 MaxSizex = fromClong ptmsx,
                                                 MaxSizey = fromClong ptmsy,
                                                 MaxPositionx = fromClong ptmpx,
                                                 MaxPositiony = fromClong ptmpy,
                                                 MinTrackSizex = fromClong pttsx,
                                                 MinTrackSizey = fromClong pttsy,
                                                 MaxTrackSizex = fromClong ptmtsx,
                                                 MaxTrackSizey = fromClong ptmtsy                                           
                                                } (* "0x0024" *)
                            end

  |  MSG_PAINTICON      =>  SYS_PAINTICON { w = h } (* "0x0026" *)

  |  MSG_ICONERASEBKGND =>  SYS_ICONERASEBKGND { w = h, 
                                                 devicecontext = MLHDC (toClong wp) } (* "0x0027" *)

  |  MSG_NEXTDLGCTL     =>  SYS_NEXTDLGCTL { w = h, 
                                             control    = wp,
                                             handleflag = itob lp  } (* "0x0028" *)

  |  MSG_SPOOLERSTATUS  =>  SYS_SPOOLERSTATUS { w = h, 
                                                jobstatus = wp,
                                                jobsleft  = LOWORD lp  } (* "0x002A" *)
 
  |  MSG_DRAWITEM       =>  let
                              val (CtlType,CtlID,itemID,itemAction,itemState, 
                                   hItem,hDC,
                                   rcleft,rctop,rcbottom,rcright,
                                   itemData) = 
                                           break_struct12 ([Cint,Cint,Cint,Cint,Cint,
                                                          voidStar,voidStar,
                                                          Clong,Clong,Clong,Clong,
                                                          Clong]) 
                                                        (deref (toClong lp)) 
                            in
                            SYS_DRAWITEM  { w = h, 
                                            controlid = wp,
                                            CtlType = fromCint CtlType,
                                            CtlID = fromCint CtlID,
                                            itemID = fromCint itemID,
                                            itemAction = fromCint itemAction,
                                            itemState = fromCint itemState,
                                            hItem = MLHWND hItem,
                                            hDC = MLHDC hDC,
                                            rcItem = mkRect {left   = fromClong rcleft,
                                                             top    = fromClong rctop,
                                                             bottom = fromClong rcbottom,
                                                             right  = fromClong rcright},
                                            itemData = fromClong itemData
                                           } (* "0x002B" *)
                            end

  |  MSG_MEASUREITEM    =>  let
                              val (CtlType,CtlID,itemID,
                                   itemWidth,itemHeight,itemData) = 
                                      break_struct6 ([Cint,Cint,Cint,
                                                     Cint,Cint,Clong])
                                                   (deref (toClong lp))
                            in
                            SYS_MEASUREITEM { w = h, 
                                              controlid = wp,
                                              CtlType = fromCint CtlType,
                                              CtlID = fromCint CtlID,
                                              itemID = fromCint itemID,
                                              itemWidth = fromCint itemWidth,
                                              itemHeight = fromCint itemHeight,
                                              itemData = fromClong itemData 
                                         } (* "0x002C" *)
                            end

  |  MSG_DELETEITEM     =>  let
                              val (CtlType,CtlID,itemID,
                                   hItem,itemData) = 
                                      break_struct5 ([Cint,Cint,Cint,voidStar,Cint])
                                                   (deref (toClong lp))
                            in
                            SYS_DELETEITEM { w = h, 
                                             controlid = wp,
                                             CtlType = fromCint CtlType,
                                             CtlID = fromCint CtlID,
                                             itemID = fromCint itemID,
                                             item = MLHWND hItem,
                                             itemData = fromCint itemData                                         
                                            } (* "0x002D" *)
                            end

  |  MSG_VKEYTOITEM     =>  SYS_VKEYTOITEM  { w = h, 
                                              virtualkey = LOWORD wp,
                                              caretpos   = HIWORD wp,
                                              listbox    = MLHWND (toClong lp)  } (* "0x002E" *)

  |  MSG_CHARTOITEM     =>  SYS_CHARTOITEM { w = h, 
                                             key = LOWORD wp,
                                             caretpos = HIWORD wp,
                                             listbox  = MLHWND (toClong lp)  } (* "0x002F" *)

  |  MSG_SETFONT        =>  SYS_SETFONT { w = h, 
                                          font       = MLHFONT (toClong wp),
                                          redrawflag = lp  } (* "0x0030" *)

  |  MSG_GETFONT        =>  SYS_GETFONT { w = h } (* "0x0031" *)

  |  MSG_SETHOTKEY      =>  SYS_SETHOTKEY { w = h, 
                                            virtualkey = wp  } (* "0x0032" *)

  |  MSG_GETHOTKEY      =>  SYS_GETHOTKEY { w = h } (* "0x0033" *)

  |  MSG_QUERYDRAGICON  =>  SYS_QUERYDRAGICON { w = h } (* "0x0037" *)

  |  MSG_COMPAREITEM    =>  let
                              val (CtlType,CtlID,
                                   hItem,
                                   itemID1,itemData1,
                                   itemID2,itemData2) =
                                      break_struct7 ([Cint,Cint,
                                                     voidStar,
                                                     Cint,Clong,Cint,Clong])
                                                   (deref (toClong lp))      
                            in
                            SYS_COMPAREITEM { w = h,
                                              controlid = wp, 
                                              CtlType = fromCint CtlType,
                                              CtlID = fromCint CtlID,
                                              hItem = MLHWND hItem,
                                              itemID1 = fromCint itemID1,
                                              itemData1 = fromClong itemData1,
                                              itemID2 = fromCint itemID2,
                                              itemData2 = fromClong itemData2
                                             } (* "0x0039" *)
                            end

  |  MSG_COMPACTING     =>  SYS_COMPACTING { w = h, 
                                             compactratio = wp } (* "0x0041" *)

  |  MSG_WINDOWPOSCHANGING    => let
                                      val (wh,front,
                                           xpos,ypos,width,height,
                                           flags) = break_struct7 ([voidStar,voidStar,
                                                                   Cint,Cint,Cint,Cint,Cint])
                                                                  (deref (toClong lp)) 
                                    in
                                    SYS_WINDOWPOSCHANGING  { w = h, 
                                                             front  = MLHWND front,
                                                             xpos   = fromCint xpos,
                                                             ypos   = fromCint ypos,
                                                             width  = fromCint width,
                                                             height = fromCint height,
                                                             flags  = absE SetWindowPositionStyleE (fromCint flags)  } (* "0x0047" *)
                                    end 

  |  MSG_WINDOWPOSCHANGED        => let
                                      val (wh,front,
                                           xpos,ypos,width,height,
                                           flags) = break_struct7 ([voidStar,voidStar,
                                                                   Cint,Cint,Cint,Cint,Cint])
                                                                 (deref (toClong lp))
                                    in
                                    SYS_WINDOWPOSCHANGED  { w = h,  
                                                            front  = MLHWND front,
                                                            xpos   = fromCint xpos,
                                                            ypos   = fromCint ypos,
                                                            width  = fromCint width,
                                                            height = fromCint height,
                                                            flags  = absE SetWindowPositionStyleE (fromCint flags)  } (* "0x0047" *)
                                    end

  |  MSG_POWER          =>  SYS_POWER { w = h, 
                                        powerevent = wp  } (* "0x0048" *)

  |  MSG_COPYDATA       =>  let
                              val (data,_,_) = 
                                        break_struct3 ([Clong,Clong,voidStar])
                                                     (deref (toClong lp))  
                            in
                            SYS_COPYDATA  { w = h, 
                                            sender = MLHWND (toClong wp),
                                            data = fromClong data                                   
                                           } (* "0x004A" *)
                            end

  |  MSG_CANCELJOURNAL  =>  SYS_CANCELJOURNAL (* "0x004B" *)

  |  MSG_NCCREATE       =>  let
                              val (createp,inst,menu,parent,
                                   cy,cx,y,x,
                                   style,name,classname,exstyle) = 
                                      break_struct12 ([voidStar,voidStar,voidStar,voidStar,
                                                     Cint,Cint,Cint,Cint,
                                                     Clong,
                                                     Cpointer Cchar,
                                                     Cpointer Cchar,
                                                     Clong]) 
                                                   (deref (toClong lp)) 
                            in
                            SYS_NCCREATE { w = h,
                                           menu = MLHMENU menu,
                                           parent = MLHWND parent,
                                           cy = fromCint cy,
                                           cx = fromCint cx,
                                           y = fromCint y,
                                           x = fromCint x,
                                           style = fromClong style ,
                                           name = fromCstring name,
                                           classname = fromCstring classname,
                                           exstyle = fromClong exstyle                    
                                      } (* "0x0081" *)
                            end

  |  MSG_NCDESTROY      =>  SYS_NCDESTROY { w = h } (* "0x0082" *)

(* This is problematic 
   - different under Windows NT and Windows 95 *)

  |  MSG_NCCALCSIZE     => (* SYS_NCCALCSIZE 
                                  { w = h, 
                                    validarea = valid,
                                    newrect = mkRect {left=call1 (getrts "getr0left") (POINTER) (LONG) psize, 
                                                      top=call1 (getrts "getr0top") (POINTER) (LONG) psize,
                                                      right=call1 (getrts "getr0right") (POINTER) (LONG) psize,
                                                      bottom=call1 (getrts "getr0bottom") (POINTER) (LONG) psize},
                                    oldrect       = mkRect {left=call1 (getrts "getr1left") (POINTER) (LONG) psize,
                                                            top=call1 (getrts "getr1top") (POINTER) (LONG) psize,
                                                            right=call1 (getrts "getr1right") (POINTER) (LONG) psize,
                                                            bottom=call1 (getrts "getr1bottom") (POINTER) (LONG) psize},
                                    oldclientarea = mkRect {left=call1 (getrts "getr2left") (POINTER) (LONG) psize,
                                                            top=call1 (getrts "getr2top") (POINTER) (LONG) psize,
                                                            right=call1 (getrts "getr2right") (POINTER) (LONG) psize,
                                                            bottom=call1 (getrts "getr2bottom") (POINTER) (LONG) psize},
                                    InsertAfter = call1 (getrts "gethwndafter") (POINTER) (HWND) psize,
                                    x = call1 (getrts "getx") (POINTER) (INT) psize,
                                    y = call1 (getrts "gety") (POINTER) (INT) psize,
                                    cx = call1 (getrts "getcx") (POINTER) (INT) psize,
                                    cy = call1 (getrts "getcy") (POINTER) (INT) psize,
                                    flags = call1 (getrts "getflags") (POINTER) (INT) psize 
                                    } *) (* "0x0083" *)

                              SYS_NCCALCSIZE 
                                  { w = h, 
                                    validarea = true,
                                    newrect = mkRect {left=0, 
                                                      top=0,
                                                      right=0,
                                                      bottom=0},
                                    oldrect       = mkRect {left=0,
                                                            top=0,
                                                            right=0,
                                                            bottom=0},
                                    oldclientarea = mkRect {left=0,
                                                            top=0,
                                                            right=0,
                                                            bottom=0},
                                    InsertAfter = MLHWND (toClong 0),
                                    x = 0,
                                    y = 0,
                                    cx = 0,
                                    cy = 0,
                                    flags = 0 
                                    } (* "0x0083" *)


  |  MSG_NCHITTEST      =>  SYS_NCHITTEST { w = h, 
                                            xpos = LOWORD lp,
                                            ypos = HIWORD lp  } (* "0x0084" *)

  |  MSG_NCPAINT        =>  SYS_NCPAINT { w = h, 
                                          region = MLHRGN (toClong wp)  } (* "0x0085" *)

  |  MSG_NCACTIVATE     =>  SYS_NCACTIVATE  { w = h, 
                                              active = itob wp } (* "0x0086" *)

  |  MSG_GETDLGCODE     =>  SYS_GETDLGCODE { w = h } (* "0x0087" *)

  |  MSG_NCMOUSEMOVE    =>  SYS_NCMOUSEMOVE { w = h, 
                                              hit_test = wp,
                                              xpos     = LOWORD lp,
                                              ypos     = HIWORD lp  } (* "0x00A0" *)

  |  MSG_NCLBUTTONDOWN  =>  SYS_NCLBUTTONDOWN  { w = h, 
                                                 hit_test = wp,
                                                 xpos     = LOWORD lp,
                                                 ypos     = HIWORD lp  }(* "0x00A1" *)

  |  MSG_NCLBUTTONUP    =>  SYS_NCLBUTTONUP  { w = h, 
                                               hit_test = wp,
                                               xpos     = LOWORD lp,
                                               ypos     = HIWORD lp  } (* "0x00A2" *)

  |  MSG_NCLBUTTONDBLCLK=>  SYS_NCLBUTTONDBLCLK { w = h, 
                                                  hit_test = wp,
                                                  xpos     = LOWORD lp,
                                                  ypos     = HIWORD lp  } (* "0x00A3" *)

  |  MSG_NCRBUTTONDOWN  =>  SYS_NCRBUTTONDOWN { w = h,  
                                                hit_test = wp,
                                                xpos     = LOWORD lp,
                                                ypos     = HIWORD lp  } (* "0x00A4" *)

  |  MSG_NCRBUTTONUP    =>  SYS_NCRBUTTONUP  { w = h, 
                                               hit_test = wp,
                                               xpos     = LOWORD lp,
                                               ypos     = HIWORD lp  } (* "0x00A5" *)

  |  MSG_NCRBUTTONDBLCLK=>  SYS_NCRBUTTONDBLCLK  { w = h, 
                                                   hit_test = wp,
                                                   xpos     = LOWORD lp,
                                                   ypos     = HIWORD lp  } (* "0x00A6" *)

  |  MSG_NCMBUTTONDOWN  =>  SYS_NCMBUTTONDOWN { w = h, 
                                                hit_test = wp,
                                                xpos     = LOWORD lp,
                                                ypos     = HIWORD lp  } (* "0x00A7" *)

  |  MSG_NCMBUTTONUP    =>  SYS_NCMBUTTONUP  { w = h, 
                                               hit_test = wp,
                                               xpos     = LOWORD lp,
                                               ypos     = HIWORD lp  } (* "0x00A8" *)

  |  MSG_NCMBUTTONDBLCLK=>  SYS_NCMBUTTONDBLCLK { w = h, 
                                                  hit_test = wp,
                                                  xpos     = LOWORD lp,
                                                  ypos     = HIWORD lp  } (* "0x00A9" *)

  |  MSG_KEYDOWN        =>  SYS_KEYDOWN { w = h, 
                                          virtualkey = wp,
                                          data       = lp  } (* "0x0100" *)

  |  MSG_KEYUP          =>  SYS_KEYUP { w = h, 
                                        virtualkey = wp,
                                        data       = lp  } (* "0x0101" *)

  |  MSG_CHAR           =>  SYS_CHAR { w = h, 
                                       ch = SML90.chr wp,
                                       data = lp  } (* "0x0102" *)

  |  MSG_DEADCHAR       =>  SYS_DEADCHAR { w = h, 
                                           charcode = SML90.chr wp,
                                           keydata  = lp   } (* "0x0103" *)

  |  MSG_SYSKEYUP       =>  SYS_SYSKEYUP { w = h, 
                                          virtualkey = wp,  
                                          keydata    = lp  } (* "0x0105" *)

  |  MSG_SYSCHAR        =>  SYS_SYSCHAR { w = h, 
                                          ch      = SML90.chr wp,
                                          keydata = lp  } (* "0x0106" *)

  |  MSG_SYSDEADCHAR    =>  SYS_SYSDEADCHAR { w = h, 
                                              ch      = SML90.chr wp,
                                              keydata = lp  } (* "0x0107" *)

  |  MSG_INITDIALOG     =>  SYS_INITDIALOG { w  = h, 
                                             dialog   = MLHWND (toClong wp),
                                             initdata = lp } (* "0x0110" *)

  |  MSG_COMMAND        =>  SYS_COMMAND { w = h, 
                                          notifycode = absE
                                                       ControlNotificationE
                                                       (HIWORD wp),
                                          wid        = LOWORD wp,
                                          control    = MLHWND (toClong lp)  } (* "0x0111" *)

  |  MSG_SYSCOMMAND     =>  SYS_SYSCOMMAND { w = h, 
                                            commandvalue = absE 
                                                           SystemCommandE wp,
                                            p = mkPoint {x= LOWORD lp,y= HIWORD lp}
                                           } (* "0x0112" *)

  |  MSG_TIMER          =>  SYS_TIMER  { w = h, 
                                         timerid = wp  } (* "0x0113" *)

  |  MSG_HSCROLL        =>  SYS_HSCROLL { w = h, 
                                          value = LOWORD wp,
                                          position = HIWORD wp,
                                          scrollbar = MLHWND (toClong lp) } (* "0x0114" *)

  |  MSG_VSCROLL        =>  SYS_VSCROLL { w = h, 
                                             value     = LOWORD wp,
                                             position  = HIWORD wp,
                                             scrollbar = MLHWND (toClong lp) } (* "0x0115" *)

  |  MSG_INITMENU       =>  SYS_INITMENU { w = h, 
                                           menu = MLHMENU (toClong wp) } (* "0x0116" *)

  |  MSG_INITMENUPOPUP  =>  SYS_INITMENUPOPUP { w = h, 
                                                menupopup  = MLHMENU (toClong wp),
                                                itemposition = LOWORD lp,
                                                issystemmenu = itob (HIWORD lp) } (* "0x0117" *)

  |  MSG_MENUSELECT     =>  SYS_MENUSELECT { w = h, 
                                             menuitem  = call1 (getrts "menuselectLW") (INT) (INT) wp,
                                             menuflags = call1 (getrts "menuselectHW") (INT) (INT) wp,
                                             menu      = MLHMENU (toClong lp) } (* "0x011F" *)

  |  MSG_MENUCHAR       =>  SYS_MENUCHAR { w = h, 
                                           ch       = SML90.chr (LOWORD wp),
                                           menuflag = HIWORD wp,
                                           menu     = MLHMENU (toClong lp)  } (* "0x0120" *)

  |  MSG_ENTERIDLE      =>  SYS_ENTERIDLE { w = h, 
                                            flag   = wp,
                                            Window = MLHWND (toClong lp) } (* "0x0121" *)

  |  MSG_CTLCOLORMSGBOX =>  SYS_CTLCOLORMSGBOX { w = h, 
                                                 displaycontext = MLHDC (toClong wp),
                                                 messagebox     = MLHWND (toClong lp)  } (* "0x0132" *)

  |  MSG_CTLCOLOREDIT   =>  SYS_CTLCOLOREDIT { w = h, 
                                               displaycontext = MLHDC (toClong wp),
                                               editcontrol    = MLHWND (toClong lp)  } (* "0x0133" *)

  |  MSG_CTLCOLORLISTBOX=>  SYS_CTLCOLORLISTBOX { w = h, 
                                                  displaycontext = MLHDC (toClong wp),
                                                  listbox        = MLHWND (toClong lp)   } (* "0x0134" *)

  |  MSG_CTLCOLORBTN    =>  SYS_CTLCOLORBTN { w = h, 
                                              displaycontext = MLHDC (toClong wp),
                                              button         = MLHWND (toClong lp)  }(* "0x0135" *)

  |  MSG_CTLCOLORDLG    =>  SYS_CTLCOLORDLG { w = h, 
                                              displaycontext = MLHDC (toClong wp),
                                              dialogbox      = MLHWND (toClong lp)  } (* "0x0136" *)

  |  MSG_CTLCOLORSCROLLBAR =>  SYS_CTLCOLORSCROLLBAR { w = h, 
                                                       displaycontext = MLHDC (toClong wp),
                                                       scrollbar      = MLHWND (toClong lp)  } (* "0x0137" *)

  |  MSG_CTLCOLORSTATIC =>  SYS_CTLCOLORSTATIC { w = h, 
                                             displaycontext = MLHDC (toClong wp),
                                             staticcontrol  = MLHWND (toClong lp)  } (* "0x0138" *)

  |  MSG_MOUSEMOVE      =>  SYS_MOUSEMOVE { w = h, 
                                            keyflags = wp,
                                            xpos     = LOWORD lp,
                                            ypos     = HIWORD lp  } (* "0x0200" *)

  |  MSG_LBUTTONDOWN    =>  SYS_LBUTTONDOWN { w = h, 
                                              keyflags = wp,
                                              xpos     = LOWORD lp,
                                              ypos     = HIWORD lp  } (* "0x0201" *)

  |  MSG_LBUTTONUP      =>  SYS_LBUTTONUP { w = h, 
                                           keyflags = wp,
                                           xpos     = LOWORD lp,
                                           ypos     = HIWORD lp  } (* "0x0202" *)

  |  MSG_LBUTTONDBLCLK  =>  SYS_LBUTTONDBLCLK { w = h, 
                                                keyflags = wp,
                                                xpos     = LOWORD lp,
                                                ypos     = HIWORD lp  } (* "0x0203" *)

  |  MSG_RBUTTONDOWN    =>  SYS_RBUTTONDOWN  { w = h, 
                                               keyflags = wp,
                                               xpos    = LOWORD lp,
                                               ypos    = HIWORD lp  } (* "0x0204" *)

  |  MSG_RBUTTONUP      =>  SYS_RBUTTONUP { w = h, 
                                            keyflags = wp,
                                            xpos    = LOWORD lp,
                                            ypos    = HIWORD lp  } (* "0x0205" *)

  |  MSG_RBUTTONDBLCLK  =>  SYS_RBUTTONDBLCLK  { w = h, 
                                                 keyflags = wp,
                                                 xpos    = LOWORD lp,
                                                 ypos    = HIWORD lp  }(* "0x0206" *)

  |  MSG_MBUTTONDOWN    =>  SYS_MBUTTONDOWN { w = h, 
                                              keyflags = wp,
                                              xpos     = LOWORD lp,
                                              ypos     = HIWORD lp  } (* "0x0207" *)

  |  MSG_MBUTTONUP      =>  SYS_MBUTTONUP { w = h, 
                                            keyflags = wp,
                                            xpos     = LOWORD lp,
                                            ypos     = HIWORD lp  } (* "0x0208" *)

  |  MSG_MBUTTONDBLCLK  =>  SYS_MBUTTONDBLCLK { w = h, 
                                                keyflags = wp,
                                                xpos     = LOWORD lp,
                                                ypos     = HIWORD lp  } (* "0x0209" *)

  |  MSG_PARENTNOTIFY   =>  SYS_PARENTNOTIFY { w = h, 
                                               eventflag = LOWORD wp,
                                               idchild   = HIWORD wp,
                                               value     = lp  } (* "0x0210" *)

  |  MSG_ENTERMENULOOP  =>  SYS_ENTERMENULOOP { w = h, 
                                                istrack= itob wp } (* "0x0211" *)

  |  MSG_EXITMENULOOP   =>  SYS_EXITMENULOOP { w = h, 
                                               istrack= itob wp } (* "0x0212" *)

  |  MSG_MDICREATE      =>  let
                              val (szClass,szTitle,hinst,
                                   x,y,cx,cy,
                                   style,lParam) = 
                                       break_struct9 ([Cpointer Cchar,
                                                      Cpointer Cchar,
                                                      voidStar,
                                                      Cint,Cint,Cint,Cint,
                                                      Clong,Clong])
                                                    (deref (toClong lp))
                            in
                            SYS_MDICREATE { w = h, 
                                            szClass = fromCstring szClass,
                                            szTitle = fromCstring szTitle,
                                            x = fromCint x,
                                            y = fromCint y,
                                            cx = fromCint cx,
                                            cy = fromCint cy,
                                            style = fromClong style,
                                            cdata = fromClong lParam                                        
                                          } (* "0x0220" *)
                            end

  |  MSG_MDIDESTROY     =>  SYS_MDIDESTROY  { w = h, 
                                              childtoclose = MLHWND (toClong wp) } (* "0x0221" *)

  |  MSG_MDIRESTORE     =>  SYS_MDIRESTORE { w = h, 
                                             childtorestore = MLHWND (toClong wp) } (* "0x0223" *)

  |  MSG_MDINEXT        =>  SYS_MDINEXT { w  = h, 
                                          child    = MLHWND (toClong wp),
                                          flagnext = itob lp  } (* "0x0224" *)

  |  MSG_MDIMAXIMIZE    =>  SYS_MDIMAXIMIZE { w = h, 
                                              childtomaximise = MLHWND (toClong wp) }  (* "0x0225" *)

  |  MSG_MDITILE        =>  SYS_MDITILE { w = h, 
                                          tilingflag = wp  } (* "0x0226" *)

  |  MSG_MDICASCADE     =>  SYS_MDICASCADE { w = h, 
                                             flag = wp  } (* "0x0227" *)

  |  MSG_MDIICONARRANGE =>  SYS_MDIICONARRANGE { w = h } (* "0x0228" *)

  |  MSG_MDIGETACTIVE   =>  SYS_MDIGETACTIVE  { w = h, 
                                                stateflag = itob (fromClong (address (toClong lp))) } (* "0x0229" *)

  |  MSG_MDISETMENU     =>  SYS_MDISETMENU { w = h, 
                                             framemenu  = MLHMENU (toClong wp),
                                             Windowmenu = MLHMENU (toClong lp) } (* "0x0230" *)

  |  MSG_DROPFILES      =>  SYS_DROPFILES { w = h, 
                                            hDrop = MLHANDLE (toClong wp) 
                                           } (* "0x0233" *)

  |  MSG_MDIREFRESHMENU =>  SYS_MDIREFRESHMENU { w = h } (* "0x0234" *)

  |  MSG_CUT            =>  SYS_CUT { w = h } (* "0x0300" *)

  |  MSG_COPY           =>  SYS_COPY { w = h } (* "0x0301" *)

  |  MSG_PASTE          =>  SYS_PASTE { w = h } (* "0x0302" *)

  |  MSG_CLEAR          =>  SYS_CLEAR { w = h } (* "0x0303" *)

  |  MSG_UNDO           =>  SYS_UNDO { w = h } (* "0x0304" *)

  |  MSG_RENDERFORMAT   =>  SYS_RENDERFORMAT { w = h, 
                                               format = wp  } (* "0x0305" *)

  |  MSG_RENDERALLFORMATS =>   SYS_RENDERALLFORMATS { w = h } (* "0x0306" *)

  |  MSG_DESTROYCLIPBOARD =>   SYS_DESTROYCLIPBOARD { w = h } (* "0x0307" *)

  |  MSG_DRAWCLIPBOARD  =>  SYS_DRAWCLIPBOARD { w = h } (* "0x0308" *)

  |  MSG_PAINTCLIPBOARD =>  SYS_PAINTCLIPBOARD { w = h, 
                                                 clipboard = MLHWND (toClong wp)  
                                               } (* "0x0309" *)

  |  MSG_VSCROLLCLIPBOARD => SYS_VSCROLLCLIPBOARD { w = h, 
                                                    viewer   = MLHWND (toClong wp),
                                                    code     = LOWORD lp,
                                                    position = HIWORD lp  } (* "0x030A" *)

  |  MSG_SIZECLIPBOARD  =>  SYS_SIZECLIPBOARD { w = h, 
                                                viewer = MLHWND (toClong lp) 
                                               } (* "0x030B" *)

  |  MSG_ASKCBFORMATNAME=>  let
                              val formatname = fromCstring (address (toClong lp)) 
                            in
                            SYS_ASKCBFORMATNAME { w = h, 
                                                  formatname = formatname  } (* "0x030C" *)
                            end

  |  MSG_CHANGECBCHAIN  =>  SYS_CHANGECBCHAIN { w = h, 
                                                removed = MLHWND (toClong wp),
                                                next    = MLHWND (toClong lp) } (* "0x030D" *)

  |  MSG_HSCROLLCLIPBOARD =>  SYS_HSCROLLCLIPBOARD { w = h, 
                                                        viewer   = MLHWND (toClong wp),
                                                        code     = LOWORD lp,
                                                        position = HIWORD lp  } (* "0x030E" *)

  |  MSG_QUERYNEWPALETTE =>  SYS_QUERYNEWPALETTE { w = h } (* "0x030F" *)

  |  MSG_PALETTEISCHANGING =>  SYS_PALETTEISCHANGING { w = h, 
                                                       changingpalette = MLHWND (toClong wp) } (* "0x0310" *)

  |  MSG_PALETTECHANGED =>  SYS_PALETTECHANGED { w = h, 
                                                 changedpalette = MLHWND (toClong wp) } (* "0x0311" *)

  |  MSG_HOTKEY         =>  SYS_HOTKEY { w = h, 
                                         id = wp } (* "0x0312" *)

  |  MSG_USER           =>  SYS_USER { w = h } (* "0x0400" *)

  | _                   =>  SYS_NULL


end


datatype callbacktype = MouseCB 
                      | ControlCB of Id
                      | TimerCB of Id
                      | KeyCB
                      | ScrollCB 
                      | RefreshCB
                      | DestroyCB
                      | OtherCB


datatype MouseButton = Left | Middle | Right | NoButton
datatype MouseAction = Up | Down | DoubleClk | MouseMove 

datatype InClientArea = Yes | No of HitTest

datatype MouseMessage = Mouse of { button : MouseButton,
                                   action : MouseAction,
                                   position : Point,
                                   inclientarea : InClientArea,
                                   ldown : bool,
                                   mdown : bool,
                                   rdown : bool,
                                   ctrldown : bool,
                                   shiftdown : bool }

datatype ControlMessage = Ctrl of { w : MLHWND, 
                                    notifycode : ControlNotification,
                                    ctlid : Id,
                                    ctl : MLHWND }
 
datatype KeyMessage = Key of {w : MLHWND,
                              key : string,
                              keydata : int,
                              dead : bool,
                              system : bool }

datatype ScrollBarOrientation = Horizontal | Vertical
datatype ScrollBarData = ScrollMove of ScrollBarType * int
                       | ScrollRedraw of MLHDC

datatype ScrollMessage = Scroll of {w : MLHWND,
                                    orientation : ScrollBarOrientation,
                                    data : ScrollBarData,
                                    shandle : MLHWND }

datatype RefreshMessage = Refresh of {w : MLHWND,
                                      inclientarea : bool}

datatype DestroyMessage = Destroy of {w : MLHWND}

datatype TimerMessage = Timer of {w : MLHWND, timer : Id}


datatype OtherMessage = Msg of System

(* system -> callbacktype *)

fun getcallbacktype m = 
case m of
   SYS_NCMOUSEMOVE     _ => MouseCB        
 | SYS_NCLBUTTONDOWN   _ => MouseCB    
 | SYS_NCLBUTTONUP     _ => MouseCB   
 | SYS_NCLBUTTONDBLCLK _ => MouseCB 
 | SYS_NCRBUTTONDOWN   _ => MouseCB 
 | SYS_NCRBUTTONUP     _ => MouseCB 
 | SYS_NCRBUTTONDBLCLK _ => MouseCB 
 | SYS_NCMBUTTONDOWN   _ => MouseCB 
 | SYS_NCMBUTTONUP     _ => MouseCB 
 | SYS_NCMBUTTONDBLCLK _ => MouseCB
 | SYS_MOUSEMOVE       _ => MouseCB
 | SYS_LBUTTONDOWN     _ => MouseCB
 | SYS_LBUTTONUP       _ => MouseCB 
 | SYS_LBUTTONDBLCLK   _ => MouseCB 
 | SYS_RBUTTONDOWN     _ => MouseCB 
 | SYS_RBUTTONUP       _ => MouseCB 
 | SYS_RBUTTONDBLCLK   _ => MouseCB 
 | SYS_MBUTTONDOWN     _ => MouseCB 
 | SYS_MBUTTONUP       _ => MouseCB
 | SYS_MBUTTONDBLCLK   _ => MouseCB

 | SYS_COMMAND {wid,...} => ControlCB (Id wid)

 | SYS_TIMER {timerid,...} => TimerCB (Id timerid)

 | SYS_CHAR           _ => KeyCB  
 | SYS_DEADCHAR       _ => KeyCB
 | SYS_SYSCHAR        _ => KeyCB  
 | SYS_SYSDEADCHAR    _ => KeyCB

 | SYS_HSCROLL           _ => ScrollCB   
 | SYS_VSCROLL           _ => ScrollCB
 | SYS_CTLCOLORSCROLLBAR _ => ScrollCB


 | SYS_NCPAINT           _ => RefreshCB     
 | SYS_PAINT             _ => RefreshCB
 
 | SYS_NCDESTROY         _ => DestroyCB         

 | _                     => OtherCB   


(* system -> TimerMessage *)

exception NotATimerMessage

fun timerMessageOf sysmsg =
case sysmsg of
 
   SYS_TIMER {w,timerid} => Timer {w = w,
                                   timer = Id timerid}
 | _ => raise NotATimerMessage


(* system -> MouseMessage *)

exception NotAMouseMessage

fun checkbit (flag,mask) = if flag < 0 then false 
                           else (natAnd(flag,mask) = mask)  

fun mouseMessageOf sysmsg =
case sysmsg of
 
   SYS_NCMOUSEMOVE     {w,hit_test,xpos,ypos} => Mouse { button = NoButton,
                                                        action = MouseMove,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = No (absE HitTestE hit_test),
                                                        ldown = false,
                                                        mdown = false,
                                                        rdown = false,
                                                        ctrldown = false,
                                                        shiftdown = false }        

 | SYS_NCLBUTTONDOWN   {w,hit_test,xpos,ypos} => Mouse { button = Left,
                                                        action = Down,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = No (absE HitTestE hit_test),
                                                        ldown = false,
                                                        mdown = false,
                                                        rdown = false,
                                                        ctrldown = false,
                                                        shiftdown = false }    

 | SYS_NCLBUTTONUP     {w,hit_test,xpos,ypos} => Mouse { button = Left,
                                                        action = Up,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = No (absE HitTestE hit_test),
                                                        ldown = false,
                                                        mdown = false,
                                                        rdown = false,
                                                        ctrldown = false,
                                                        shiftdown = false }   

 | SYS_NCLBUTTONDBLCLK {w,hit_test,xpos,ypos} => Mouse { button = Left,
                                                        action = DoubleClk,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = No (absE HitTestE hit_test),
                                                        ldown = false,
                                                        mdown = false,
                                                        rdown = false,
                                                        ctrldown = false,
                                                        shiftdown = false } 

 | SYS_NCRBUTTONDOWN   {w,hit_test,xpos,ypos} => Mouse { button = Right,
                                                        action = Down,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = No (absE HitTestE hit_test),
                                                        ldown = false,
                                                        mdown = false,
                                                        rdown = false,
                                                        ctrldown = false,
                                                        shiftdown = false } 

 | SYS_NCRBUTTONUP     {w,hit_test,xpos,ypos} => Mouse { button = Right,
                                                        action = Up,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = No (absE HitTestE hit_test),
                                                        ldown = false,
                                                        mdown = false,
                                                        rdown = false,
                                                        ctrldown = false,
                                                        shiftdown = false } 

 | SYS_NCRBUTTONDBLCLK {w,hit_test,xpos,ypos} => Mouse { button = Right,
                                                        action = DoubleClk,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = No (absE HitTestE hit_test),
                                                        ldown = false,
                                                        mdown = false,
                                                        rdown = false,
                                                        ctrldown = false,
                                                        shiftdown = false } 

 | SYS_NCMBUTTONDOWN   {w,hit_test,xpos,ypos} => Mouse { button = Middle,
                                                        action = Down,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = No (absE HitTestE hit_test),
                                                        ldown = false,
                                                        mdown = false,
                                                        rdown = false,
                                                        ctrldown = false,
                                                        shiftdown = false } 

 | SYS_NCMBUTTONUP     {w,hit_test,xpos,ypos} => Mouse { button = Middle,
                                                        action = Up,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = No (absE HitTestE hit_test),
                                                        ldown = false,
                                                        mdown = false,
                                                        rdown = false,
                                                        ctrldown = false,
                                                        shiftdown = false } 

 | SYS_NCMBUTTONDBLCLK {w,hit_test,xpos,ypos} => Mouse { button = Middle,
                                                        action = DoubleClk,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = No (absE HitTestE hit_test),
                                                        ldown = false,
                                                        mdown = false,
                                                        rdown = false,
                                                        ctrldown = false,
                                                        shiftdown = false }

 | SYS_MOUSEMOVE       {w,keyflags,xpos,ypos} => Mouse { button = NoButton,
                                                        action = MouseMove,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = Yes,
                                                        ldown = false,
                                                        mdown = false,
                                                        rdown = false,
                                                        ctrldown = false,
                                                        shiftdown = false }

 | SYS_LBUTTONDOWN     {w,keyflags,xpos,ypos} => Mouse { button = Left,
                                                        action = Down,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = Yes,
                                                        ldown = true,
                                                        mdown = checkbit (keyflags,16),
                                                        rdown = checkbit (keyflags,2),
                                                        ctrldown = checkbit (keyflags,8),
                                                        shiftdown = checkbit (keyflags,4) }

 | SYS_LBUTTONUP       {w,keyflags,xpos,ypos} => Mouse { button = Left,
                                                        action = Up,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = Yes,
                                                        ldown = false,
                                                        mdown = checkbit (keyflags,16),
                                                        rdown = checkbit (keyflags,2),
                                                        ctrldown = checkbit (keyflags,8),
                                                        shiftdown = checkbit (keyflags,4) }
                                                         
 | SYS_LBUTTONDBLCLK   {w,keyflags,xpos,ypos} => Mouse { button = Left,
                                                        action = DoubleClk,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = Yes,
                                                        ldown = checkbit (keyflags,1),
                                                        mdown = checkbit (keyflags,16),
                                                        rdown = checkbit (keyflags,2),
                                                        ctrldown = checkbit (keyflags,8),
                                                        shiftdown = checkbit (keyflags,4) } 

 | SYS_RBUTTONDOWN     {w,keyflags,xpos,ypos} => Mouse { button = Right,
                                                        action = Down,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = Yes,
                                                        ldown = checkbit (keyflags,1),
                                                        mdown = checkbit (keyflags,16),
                                                        rdown = true,
                                                        ctrldown = checkbit (keyflags,8),
                                                        shiftdown = checkbit (keyflags,4) } 

 | SYS_RBUTTONUP       {w,keyflags,xpos,ypos} => Mouse { button = Left,
                                                        action = Up,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = Yes,
                                                        ldown = checkbit (keyflags,1),
                                                        mdown = checkbit (keyflags,16),
                                                        rdown = true,
                                                        ctrldown = checkbit (keyflags,8),
                                                        shiftdown = checkbit (keyflags,4) } 

 | SYS_RBUTTONDBLCLK   {w,keyflags,xpos,ypos} => Mouse { button = Right,
                                                        action = DoubleClk,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = Yes,
                                                        ldown = checkbit (keyflags,1),
                                                        mdown = checkbit (keyflags,16),
                                                        rdown = checkbit (keyflags,2),
                                                        ctrldown = checkbit (keyflags,8),
                                                        shiftdown = checkbit (keyflags,4) } 

 | SYS_MBUTTONDOWN     {w,keyflags,xpos,ypos} => Mouse { button = Middle,
                                                        action = Down,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = Yes,
                                                        ldown = checkbit (keyflags,1),
                                                        mdown = true,
                                                        rdown = checkbit (keyflags,2),
                                                        ctrldown = checkbit (keyflags,8),
                                                        shiftdown = checkbit (keyflags,4) } 

 | SYS_MBUTTONUP       {w,keyflags,xpos,ypos} => Mouse { button = Middle,
                                                        action = Up,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = Yes,
                                                        ldown = checkbit (keyflags,1),
                                                        mdown = true,
                                                        rdown = checkbit (keyflags,2),
                                                        ctrldown = checkbit (keyflags,8),
                                                        shiftdown = checkbit (keyflags,4) }

 | SYS_MBUTTONDBLCLK   {w,keyflags,xpos,ypos} => Mouse { button = Middle,
                                                        action = DoubleClk,
                                                        position = mkPoint {x=xpos,y=ypos},
                                                        inclientarea = Yes,
                                                        ldown = checkbit (keyflags,1),
                                                        mdown = checkbit (keyflags,16),
                                                        rdown = checkbit (keyflags,2),
                                                        ctrldown = checkbit (keyflags,8),
                                                        shiftdown = checkbit (keyflags,4) }

 | _ => raise NotAMouseMessage

(* system -> ControlMessage *)

exception NotAControlMessage

fun controlMessageOf sysmsg = 
case sysmsg of
   SYS_COMMAND {w,notifycode,wid,control} => Ctrl { w = w, 
                                                    notifycode = notifycode,
                                                    ctlid = Id wid,
                                                    ctl = control }
 | _ => raise NotAControlMessage 

(* system -> KeyMessage *)

exception NotAKeyMessage

fun keyMessageOf sysmsg =
case sysmsg of 

   SYS_CHAR        {w,ch,data}    => Key {w = w,
                                         key = ch,
                                         keydata = data,
                                         dead = false,
                                         system = false }  
 | SYS_DEADCHAR    {w,charcode,keydata}    =>   Key {w = w,
                                         key = charcode,
                                         keydata = keydata,
                                         dead = true,
                                         system = false }
 | SYS_SYSCHAR     {w,ch,keydata} => Key {w = w,
                                         key = ch,
                                         keydata = keydata,
                                         dead = false,
                                         system = true }  
 | SYS_SYSDEADCHAR {w,ch,keydata} => Key {w = w,
                                         key = ch,
                                         keydata = keydata,
                                         dead = true,
                                         system = true }
 | _ => raise NotAKeyMessage


(* system -> KeyMessage *)

exception NotAScrollMessage

fun scrollMessageOf sysmsg =
case sysmsg of

   SYS_HSCROLL {w,value,position,scrollbar} => Scroll {w = w,
                                                       orientation = Horizontal,
                                                       data = ScrollMove 
                                                       ( (absE ScrollBarTypeE value),
                                                          position ),
                                                     shandle = scrollbar }   

 | SYS_VSCROLL {w,value,position,scrollbar} => Scroll {w = w,
                                                       orientation = Vertical,
                                                       data = ScrollMove 
                                                       ( (absE ScrollBarTypeE value),
                                                          position ),
                                                     shandle = scrollbar }

 | SYS_CTLCOLORSCROLLBAR {w,displaycontext,scrollbar} => 
                                             Scroll {w = w,
                                                     orientation = Vertical,
                                                     data = ScrollRedraw displaycontext,
                                                     shandle = scrollbar }

 | _ => raise NotAScrollMessage 


(* system -> RefreshMessage *)

exception NotARefreshMessage

fun refreshMessageOf sysmsg =
case sysmsg of
   SYS_PAINT {w}   => Refresh {w=w,inclientarea=true} 
 | SYS_NCPAINT {w,region} => Refresh {w=w,inclientarea=false}
 | _ => raise NotARefreshMessage


(* system -> DestroyMessage *)

exception NotADestroyMessage

fun destroyMessageOf sysmsg =
case sysmsg of
   SYS_NCDESTROY {w}   => Destroy {w=w} 
 | _                   => raise NotADestroyMessage


(* system -> OtherMessage *)

exception NotAGeneralMessage

fun otherMessageOf sysmsg =
case (getcallbacktype sysmsg) of
   OtherCB => Msg sysmsg 
 | _       => raise NotAGeneralMessage


end

end;
