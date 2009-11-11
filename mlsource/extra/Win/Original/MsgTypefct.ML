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

signature importTypeSig =
sig
   type MLHWND
   type MLHDC
   type MLHMENU
   type MLHANDLE
   type MLHRGN
   type MLHFONT
   type Point
   type Rectangle
end;

signature importConstTypeSig =
sig
   type WindowStyleExtended
   type WindowStyle
   type SetWindowPositionStyle
   type SystemCommand
   type ControlNotification
end;


functor MsgTypeFct ( structure Type : importTypeSig
                     structure ConstType : importConstTypeSig
                   ) =
struct

open ConstType
open Type

                                           
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


  




