(*
    Copyright (c) 2001-7
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

(* Test routine for dialogue.  Tests various messages. *)

fun dlgProc (h, Message.WM_INITDIALOG _, ()) = (Message.LRESINT 1, ())
 |  dlgProc (h, Message.WM_COMMAND{notifyCode = 0, wId, ...}, ()) =
        (if wId = MessageBox.IDOK orelse wId = MessageBox.IDCANCEL
         then (Dialog.EndDialog(h, wId); Message.PostQuitMessage 0) else ();
         (Message.LRESINT 1, ()))
 |  dlgProc msg = ((*PolyML.print msg;*)(Message.LRESINT 0, ()));

val dial = ref Base.hwndNull;

local
open Dialog Window.Style
in
val template =
   {x = 0, y = 0, cx = 215, cy = 135, font = SOME (8, "MS Sans Serif"), menu = NONE,
    class = NONE, style = flags[WS_POPUP, WS_CAPTION, WS_SYSMENU], title = "Dialogue",
    extendedStyle = 0,
    items =
      [{x = 158, y = 7, cx = 50, cy = 14, id = 1, creationData = NONE, extendedStyle = 0,
        class = DLG_BUTTON(flags[WS_CHILD, WS_VISIBLE, WS_TABSTOP]), title = DLG_TITLESTRING "OK"},
       {x = 158, y = 24, cx = 50, cy = 14, id = 2,creationData = NONE, extendedStyle = 0,
        class = DLG_BUTTON(flags[WS_CHILD, WS_VISIBLE, WS_TABSTOP]), title = DLG_TITLESTRING "Cancel"},
       {x = 45, y = 67, cx = 48, cy = 61, id = 1003, title = DLG_TITLESTRING "",
        creationData = NONE, extendedStyle = 0,
        class = DLG_COMBOBOX(flags[WS_CHILD, WS_VISIBLE, WS_VSCROLL, WS_TABSTOP])},
       {x = 23, y = 26, cx = 19, cy = 8, id = 65535, creationData = NONE, extendedStyle = 0,
        class = DLG_STATIC(flags[WS_CHILD, WS_VISIBLE, WS_GROUP]), title = DLG_TITLESTRING "Static"},
       {x = 64, y = 24, cx = 40, cy = 14, id = 1000, creationData = NONE, extendedStyle = 0,
        class = DLG_EDIT(flags[WS_CHILD, WS_VISIBLE, WS_BORDER, WS_TABSTOP]), title = DLG_TITLESTRING ""},
       {x = 14, y = 47, cx = 103, cy = 11, id = 1001, creationData = NONE, extendedStyle = 0,
        class = DLG_SCROLLBAR(flags[WS_CHILD, WS_VISIBLE]), title = DLG_TITLESTRING ""},
       {x = 136, y = 46, cx = 58, cy = 72, id = 1002, creationData = NONE, extendedStyle = 0,
        class = DLG_LISTBOX (flags[WS_CHILD, WS_VISIBLE, WS_BORDER, WS_VSCROLL, WS_TABSTOP]),
        title = DLG_TITLESTRING ""}]}
fun makedial() =
    CreateDialogIndirect(Globals.ApplicationInstance(), template,
        Globals.MainWindow(), dlgProc, ());
end;
(*
val hi = Resource.LoadLibrary "C:\\Source Files\\DialogueDLL\\Debug\\DialogueDLL.dll";
     
fun makedial() = Dialog.CreateDialog(hi, Resource.IdAsString "MYDIALOGUE", Globals.MainWindow(),
    dlgProc, ());
*)
(* The dialogue has to be created by the thread that will handle its messages. *)
fun runDialogue() =
    (
        dial := makedial();
        Window.ShowWindow(!dial, Window.SW_SHOW);
        Window.SetForegroundWindow (!dial);
        Message.RunApplication();
        ()
    );

Thread.Thread.fork(runDialogue, []);

val combo = Dialog.GetDlgItem(!dial, 1003);
val scroll = Dialog.GetDlgItem(!dial, 1001);
val listbox = Dialog.GetDlgItem(!dial, 1002);

val info = ref {minPos = 10, maxPos = 20, pageSize = 4, pos = 15, trackPos = 0};
Message.SendMessage(scroll, Message.SBM_SETSCROLLINFO{info= !info, options=Scrollbar.SIF_ALL});
Message.SendMessage(scroll, Message.SBM_GETSCROLLINFO{info=info, options=Scrollbar.SIF_ALL});
!info;
Message.SendMessage(combo, Message.CB_DIR{attrs = [], fileSpec ="C:\\*"});

Message.SendMessage(combo, Message.CB_DIR{attrs = [], fileSpec ="C:\\*"});
