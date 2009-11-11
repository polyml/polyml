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


(****************************************************************

  The message value datatype

 ****************************************************************)

functor MsgValFct () = 
struct
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



fun i2MessageValue i = 
case i of
    1 => MSG_CREATE
  | 2 => MSG_DESTROY
  | 3 => MSG_MOVE
  | 5 => MSG_SIZE
  | 6 => MSG_ACTIVATE
  | 7 => MSG_SETFOCUS
  | 8 => MSG_KILLFOCUS
  | 10 => MSG_ENABLE
  | 11 => MSG_SETREDRAW
  | 12 => MSG_SETTEXT
  | 13 => MSG_GETTEXT
  | 14 => MSG_GETTEXTLENGTH
  | 15 => MSG_PAINT
  | 16 => MSG_CLOSE
  | 17 => MSG_QUERYENDSESSION
  | 18 => MSG_QUIT
  | 19 => MSG_QUERYOPEN
  | 20 => MSG_ERASEBKGND
  | 21 => MSG_SYSCOLORCHANGE
  | 22 => MSG_ENDSESSION
  | 24 => MSG_SHOWWINDOW
  | 26 => MSG_WININICHANGE
  | 27 => MSG_DEVMODECHANGE
  | 28 => MSG_ACTIVATEAPP
  | 29 => MSG_FONTCHANGE
  | 30 => MSG_TIMECHANGE
  | 31 => MSG_CANCELMODE
  | 32 => MSG_SETCURSOR
  | 33 => MSG_MOUSEACTIVATE
  | 34 => MSG_CHILDACTIVATE
  | 35 => MSG_QUEUESYNC
  | 36 => MSG_GETMINMAXINFO
  | 38 => MSG_PAINTICON
  | 39 => MSG_ICONERASEBKGND
  | 40 => MSG_NEXTDLGCTL
  | 42 => MSG_SPOOLERSTATUS
  | 43 => MSG_DRAWITEM
  | 44 => MSG_MEASUREITEM
  | 45 => MSG_DELETEITEM
  | 46 => MSG_VKEYTOITEM
  | 47 => MSG_CHARTOITEM
  | 48 => MSG_SETFONT
  | 49 => MSG_GETFONT
  | 50 => MSG_SETHOTKEY
  | 51 => MSG_GETHOTKEY
  | 55 => MSG_QUERYDRAGICON
  | 57 => MSG_COMPAREITEM
  | 65 => MSG_COMPACTING
  | 66 => MSG_OTHERWINDOWCREATED
  | 67 => MSG_OTHERWINDOWDESTROYED
  | 68 => MSG_COMMNOTIFY
  | 70 => MSG_WINDOWPOSCHANGING
  | 71 => MSG_WINDOWPOSCHANGED
  | 72 => MSG_POWER
  | 74 => MSG_COPYDATA
  | 75 => MSG_CANCELJOURNAL
  | 129 => MSG_NCCREATE
  | 130 => MSG_NCDESTROY
  | 131 => MSG_NCCALCSIZE
  | 132 => MSG_NCHITTEST
  | 133 => MSG_NCPAINT
  | 134 => MSG_NCACTIVATE
  | 135 => MSG_GETDLGCODE
  | 160 => MSG_NCMOUSEMOVE
  | 161 => MSG_NCLBUTTONDOWN
  | 162 => MSG_NCLBUTTONUP
  | 163 => MSG_NCLBUTTONDBLCLK
  | 164 => MSG_NCRBUTTONDOWN
  | 165 => MSG_NCRBUTTONUP
  | 166 => MSG_NCRBUTTONDBLCLK
  | 167 => MSG_NCMBUTTONDOWN
  | 168 => MSG_NCMBUTTONUP
  | 169 => MSG_NCMBUTTONDBLCLK
  | 256 => MSG_KEYDOWN
  | 257 => MSG_KEYUP
  | 258 => MSG_CHAR
  | 259 => MSG_DEADCHAR
  | 260 => MSG_SYSKEYDOWN
  | 261 => MSG_SYSKEYUP
  | 262 => MSG_SYSCHAR
  | 263 => MSG_SYSDEADCHAR
  | 264 => MSG_KEYLAST
  | 272 => MSG_INITDIALOG
  | 273 => MSG_COMMAND
  | 274 => MSG_SYSCOMMAND
  | 275 => MSG_TIMER
  | 276 => MSG_HSCROLL
  | 277 => MSG_VSCROLL
  | 278 => MSG_INITMENU
  | 279 => MSG_INITMENUPOPUP
  | 287 => MSG_MENUSELECT
  | 288 => MSG_MENUCHAR
  | 289 => MSG_ENTERIDLE
  | 306 => MSG_CTLCOLORMSGBOX
  | 307 => MSG_CTLCOLOREDIT
  | 308 => MSG_CTLCOLORLISTBOX
  | 309 => MSG_CTLCOLORBTN
  | 310 => MSG_CTLCOLORDLG
  | 311 => MSG_CTLCOLORSCROLLBAR
  | 312 => MSG_CTLCOLORSTATIC
  | 512 => MSG_MOUSEMOVE
  | 513 => MSG_LBUTTONDOWN
  | 514 => MSG_LBUTTONUP
  | 515 => MSG_LBUTTONDBLCLK
  | 516 => MSG_RBUTTONDOWN
  | 517 => MSG_RBUTTONUP
  | 518 => MSG_RBUTTONDBLCLK
  | 519 => MSG_MBUTTONDOWN
  | 520 => MSG_MBUTTONUP
  | 521 => MSG_MBUTTONDBLCLK
  | 528 => MSG_PARENTNOTIFY
  | 529 => MSG_ENTERMENULOOP
  | 530 => MSG_EXITMENULOOP
  | 544 => MSG_MDICREATE
  | 545 => MSG_MDIDESTROY
  | 546 => MSG_MDIACTIVATE
  | 547 => MSG_MDIRESTORE
  | 548 => MSG_MDINEXT
  | 549 => MSG_MDIMAXIMIZE
  | 550 => MSG_MDITILE
  | 551 => MSG_MDICASCADE
  | 552 => MSG_MDIICONARRANGE
  | 553 => MSG_MDIGETACTIVE
  | 560 => MSG_MDISETMENU
  | 563 => MSG_DROPFILES
  | 564 => MSG_MDIREFRESHMENU
  | 768 => MSG_CUT
  | 769 => MSG_COPY
  | 770 => MSG_PASTE
  | 771 => MSG_CLEAR
  | 772 => MSG_UNDO
  | 773 => MSG_RENDERFORMAT
  | 774 => MSG_RENDERALLFORMATS
  | 775 => MSG_DESTROYCLIPBOARD
  | 776 => MSG_DRAWCLIPBOARD
  | 777 => MSG_PAINTCLIPBOARD
  | 778 => MSG_VSCROLLCLIPBOARD
  | 779 => MSG_SIZECLIPBOARD
  | 780 => MSG_ASKCBFORMATNAME
  | 781 => MSG_CHANGECBCHAIN
  | 782 => MSG_HSCROLLCLIPBOARD
  | 783 => MSG_QUERYNEWPALETTE
  | 784 => MSG_PALETTEISCHANGING
  | 785 => MSG_PALETTECHANGED
  | 786 => MSG_HOTKEY
  | 896 => MSG_PENWINFIRST
  | 911 => MSG_PENWINLAST
  | 1024 => MSG_USER
  | _    => MSG_NULL

end;

