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
(* Example text editor. *)
fun mlEdit () =
let
    open Window Message Menu Edit Class Dialog CommonDialog MessageBox Caret
    open DeviceContext Font Printing Transform Painting Color
    open Keyboard

    (* Define values to be delivered when the menu items are selected.
       The Id is delivered as part of a WM_COMMAND message. *)
    val menuOpen = 1
    and menuQuit = 2
    and menuSave = 3
    and menuSaveAs = 4
    and menuCut = 5
    and menuCopy = 6
    and menuPaste = 7
    and menuFind = 8
    and menuPageSetup = 9
    and menuPrint = 10
    and menuAbout = 11

    val app = Globals.ApplicationInstance()

    (* Borrow the Poly icon from the application program. It happens to
       be icon id 102. If this doesn't work return NULL. *)
    val polyIcon =
        Icon.LoadIcon(app, Resource.MAKEINTRESOURCE 102) handle _ => Globals.hNull;

    local
        (* Create sub-menus. *)
        val fileMenu =
            let
                val fileMenu = CreateMenu();
            in
                AppendMenu(fileMenu, [], MenuId menuOpen, MFT_STRING "&Open");
                AppendMenu(fileMenu, [], MenuId menuSave, MFT_STRING "&Save");
                AppendMenu(fileMenu, [], MenuId menuSaveAs, MFT_STRING "Save &As...");
                AppendMenu(fileMenu, [], MenuId 0, MFT_SEPARATOR);
                AppendMenu(fileMenu, [], MenuId menuPageSetup, MFT_STRING "Page Set&up...");
                AppendMenu(fileMenu, [], MenuId menuPrint, MFT_STRING "P&rint...");
                AppendMenu(fileMenu, [], MenuId 0, MFT_SEPARATOR);
                AppendMenu(fileMenu, [], MenuId menuQuit, MFT_STRING "&Quit");
                fileMenu
            end;
    
        val editMenu =
            let
                val editMenu = CreateMenu();
            in
                AppendMenu(editMenu, [], MenuId menuCut, MFT_STRING "Cu&t");
                AppendMenu(editMenu, [], MenuId menuCopy, MFT_STRING "&Copy");
                AppendMenu(editMenu, [], MenuId menuPaste, MFT_STRING "&Paste");
                AppendMenu(editMenu, [], MenuId menuFind, MFT_STRING "&Find");
                editMenu
            end;

        val helpMenu =
            let
                val helpMenu = CreateMenu()
            in
                AppendMenu(helpMenu, [], MenuId menuAbout, MFT_STRING "&About mlEdit...");
                helpMenu
            end

    in
        (* Create the main menu and append the sub-menus. *)
        val menu = CreateMenu();
        val _ = AppendMenu(menu, [], MenuHandle fileMenu, MFT_STRING "&File");
        val _ = AppendMenu(menu, [], MenuHandle editMenu, MFT_STRING "&Edit")
        val _ = AppendMenu(menu, [], MenuHandle helpMenu, MFT_STRING "&Help")
    end;

    (* The "state" of the editor. *)
    type state = {
        edit: HWND, (* Handle to the edit window. *)
        devMode: DEVMODE option, devNames: DEVNAMES option, (* Printer settings *)
        fileName: string
        }

    fun wndProc(hw: HWND, msg: Message, NONE): LRESULT * state option =
        (
        case msg of
            WM_CREATE _ => (* Create an edit window and return it as the state. *)
            let
                val edit =
                 CreateWindow{class = Class.Edit, name = "",
                    (* The style does not include horizontal scrolling.  That causes us to use word-wrapping. *)
                    style = Edit.Style.flags[Edit.Style.WS_CHILD, Edit.Style.WS_VISIBLE, Edit.Style.WS_VSCROLL,
                                    (*Edit.Style.WS_HSCROLL, *)Edit.Style.ES_LEFT, Edit.Style.ES_MULTILINE,
                                    Edit.Style.ES_AUTOVSCROLL(*, Edit.Style.ES_AUTOHSCROLL*)],
                    x  = 0, y = 0, height = 0, width = 0, relation = ChildWindow{parent=hw, id=99},
                    instance = Globals.ApplicationInstance(), init = ()}

                (* Create a 10 point Courier font. *)
                val hDC = GetDC edit;
                val height = ~10 * GetDeviceCaps(hDC, LOGPIXELSY) div 72;
                val _ = ReleaseDC(edit, hDC);
                val hFont = CreateFont{height=height, width=0, escapement=0, orientation=0,
                       weight=FW_DONTCARE, italic=false, underline=false, strikeOut=false,
                       charSet=ANSI_CHARSET, outputPrecision=OUT_DEFAULT_PRECIS,
                       clipPrecision=CLIP_DEFAULT_PRECIS, quality=DEFAULT_QUALITY,
                       pitch=FIXED_PITCH, family=FF_MODERN, faceName="Courier"}
            in
                SendMessage(edit, WM_SETFONT{font=hFont, redrawflag=false});
                (LRESINT 0, SOME{edit=edit, devMode=NONE, devNames = NONE, fileName=""})
            end

        | _ => (DefWindowProc(hw, msg), NONE)
        )

    | wndProc(hw: HWND,
              msg: Message,
              state: state option as SOME{edit, devMode, devNames, fileName, ...}) =
        case msg of
            WM_SETFOCUS _ =>
                (* If we get a focus request we set the focus to the edit window. *)
                (SetFocus(SOME edit); (DefWindowProc(hw, msg), state))
    
        |    WM_SIZE{height, width, ...} =>
                (* If we get a size change we set the size of the edit window. *)
                (MoveWindow{hWnd=edit, x=0, y=0, height=height, width=width, repaint=true}; (DefWindowProc(hw, msg), state))
    
        |    WM_NCDESTROY =>
                (* When the window is closed we send a QUIT message which exits from the application loop. *)
                (PostQuitMessage 0; (DefWindowProc(hw, msg), state))
    
        |    WM_CLOSE =>
                (* User has pressed the Close box.  If it's ok to close we could call
                   DestroyWindow ourselves.  Just as an example we return NONE which
                   passes it to the default window procedure and does it for us. *)
                (if checkForSave(hw, edit, fileName) then DefWindowProc(hw, msg) else LRESINT 0, state)
    
        |    WM_COMMAND{notifyCode = 0, wId, control} =>
                (* Menu selections arrive here. *)

                if wId = menuQuit
                then
                    (
                    if checkForSave(hw, edit, fileName) then DestroyWindow hw else ();
                    (LRESINT 0, state)
                    )

                else if wId = menuOpen
                then
                let
                    val on = {
                        owner = SOME hw,
                        template = TemplateDefault,
                        filter =
                            [("Text Files (*.txt)", "*.txt"),
                             ("ML Files (*.sml)", "*.sml"),
                             ("All Files (*.*)", "*.*")],
                        customFilter = NONE,
                        filterIndex = 1,
                        file = "",
                        maxFile = 1000,
                        fileTitle = "",
                        initialDir = NONE,
                        title = NONE,
                        flags = OpenFileFlags.flags[OpenFileFlags.OFN_HIDEREADONLY],
                        defExt = NONE
                    }
                in
                    case GetOpenFileName on of
                        NONE => (LRESINT 0, state)
                    |    SOME {file, ...} =>
                        (* If it's been modified we need to ask before overwriting. *)
                        if checkForSave(hw, edit, fileName)
                        then
                        (let
                            val f = TextIO.openIn file
                            (* Text input will convert CRNL to \n.  We need to
                               reverse the process. *)
                            fun nlToCrnl s =
                                String.translate(fn #"\n" => "\r\n" | c => String.str c) s
                        in
                            (* Should we save any existing file? *)
                            SetWindowText(edit, nlToCrnl(TextIO.inputAll f));
                            TextIO.closeIn f;
                            SendMessage(edit, EM_SETMODIFY{modified=false});
                            (LRESINT 0, SOME{edit=edit, devMode=devMode, devNames=devNames,
                                          fileName=file})
                        end) handle exn =>
                            (MessageBox(SOME hw,
                                concat["Unable to open - ", file, "\n"(*, exnMessage exn*)],
                                "Open failure", MessageBoxStyle.MB_OK);
                            (LRESINT 0, state))
                        else (LRESINT 0, state)
                end

                else if wId = menuSave andalso fileName <> ""
                then (* Save to the original file name if there is one. *)
                (
                    saveDocument(hw, fileName, edit);
                    (LRESINT 0, state)
                )

                else if wId = menuSaveAs orelse wId = menuSave (* andalso fileName = "" *)
                then
                (
                    case saveAsDocument(hw, edit) of
                        NONE => (LRESINT 0, state)
                    |    SOME newName =>
                            (LRESINT 0, (* Use the selected file name. *)
                                SOME{edit=edit, devMode=devMode, devNames=devNames,
                                     fileName=newName})
                )

                else if wId = menuFind
                then
                let
                    open FindReplaceFlags
                    (* Create a "Find" dialogue. *)
                    val find =
                        FindText{owner = hw, template = TemplateDefault,
                                 flags=flags[FR_DOWN, FR_HIDEWHOLEWORD],
                                 findWhat="", replaceWith="", bufferSize = 100}
                in
                    ShowWindow(find, SW_SHOW);
                    (LRESINT 0, state)
                end

                (* Cut, Copy and Paste are all handled by the Edit window. *)
                else if wId = menuCut
                then (SendMessage(edit, WM_CUT); (LRESINT 0, state))
                else if wId = menuCopy
                then (SendMessage(edit, WM_COPY); (LRESINT 0, state))
                else if wId = menuPaste
                then (SendMessage(edit, WM_PASTE); (LRESINT 0, state))

                else if wId = menuPageSetup
                then
                    (
                    (* Put up the dialogue and change the settings if necessary. *)
                    case PageSetupDlg {owner=SOME hw, devMode=devMode, devNames=devNames,
                                     flags=PageSetupFlags.flags[], paperSize={x=0,y=0},
                                     minMargin={top=0,bottom=0,left=0,right=0},
                                     margin={top=0,bottom=0,left=0,right=0}} of
                        NONE => (LRESINT 0, state)
                    |    SOME {devMode, devNames, ...} =>
                            (LRESINT 0, SOME{edit=edit, devMode=devMode, devNames=devNames,
                                          fileName=fileName})
                    )

                else if wId = menuPrint (* "Print" menu item. *)
                then
                let
                    (* Put up the dialogue box to get the settings. *)
                    val printSettings =
                        PrintDlg {owner=SOME hw, devMode=devMode, devNames=devNames,
                                  context=NONE,
                                  flags=PrintDlgFlags.flags[PrintDlgFlags.PD_RETURNDC],
                                  fromPage=1, toPage= ~1, minPage=1, maxPage= ~1, copies=1};
                in
                    case printSettings of
                        SOME {devMode, devNames, context = SOME hdc, flags, fromPage, toPage, ...} =>
                        (let
                            (* If the "Selection" button has been pressed we only print the
                               selection. *)
                            val printWhat =
                                if PrintDlgFlags.anySet(flags, PrintDlgFlags.PD_SELECTION)
                                then
                                let
                                    val from = ref 0 and to = ref 0
                                    val _ = SendMessage(edit, EM_GETSEL{startPos = from, endPos = to})
                                    val text = GetWindowText edit
                                in
                                    if !from < 0 orelse !from > size text orelse
                                       !to < 0 orelse !from > size text
                                    then ""
                                    else String.substring(text, !from, !to - !from)
                                end
                                else (* "All" button pressed or "Pages" pressed. *)
                                    GetWindowText edit;
                            val textLength = size printWhat

                            (* Tell the spooler to start the document. *)
                            val jobID = StartDoc(hdc, {docName=fileName, output=NONE, dType=NONE})

                            (* Find out how big a character is. From this we can work out
                               how many characters fit on a line and how many lines on a
                               page. Since we're using a fixed width font this is fairly
                               easy. *)
                            val _ = SetMapMode(hdc, MM_TEXT)
                            val white = RGB{red=255, blue=255, green=255}
                            val black = RGB{red=0, blue=0, green = 0}
                            val pageWidth = GetDeviceCaps(hdc, HORZRES)
                            and pageHeight = GetDeviceCaps(hdc, VERTRES)

                            (* Create the same font as we're using on the screen. Since this is
                               a fixed width font it makes calculating the number of characters
                               fairly easy. *)
                            val charHeight = ~10 * GetDeviceCaps(hdc, LOGPIXELSY) div 72;
                            val hFont = CreateFont{height=charHeight, width=0, escapement=0, orientation=0,
                                   weight=FW_DONTCARE, italic=false, underline=false, strikeOut=false,
                                   charSet=ANSI_CHARSET, outputPrecision=OUT_DEFAULT_PRECIS,
                                   clipPrecision=CLIP_DEFAULT_PRECIS, quality=DEFAULT_QUALITY,
                                   pitch=FIXED_PITCH, family=FF_MODERN, faceName="Courier"}
                            val oldFont = SelectObject(hdc, hFont); (* Use this font. *)

                            val textMetric = GetTextMetrics hdc;

                            fun printPage pno index =
                            let
                                (* If we are printing a range of pages we need to check whether
                                   we are in the range. *)
                                val printThisPage =
                                    if PrintDlgFlags.anySet(flags, PrintDlgFlags.PD_PAGENUMS)
                                    then pno >= fromPage andalso (pno <= toPage orelse toPage < 0)
                                    else true
                                val pageRect = {top=0, left=0, bottom=pageHeight, right=pageWidth}
                                (* Calculate the number of lines and columns. *)
                                val nLines = pageHeight div #height textMetric;
                                val nCols = pageWidth div #maxCharWidth textMetric

                                (* Output the lines to fill the page. *)
                                fun outputLines lineNo p =
                                    if lineNo >= nLines
                                    then p (* Return last pointer. *)
                                    else
                                    let
                                        (* Find the point to split the line.  We stop at the end of
                                           the text, a line break, the last word break on the line
                                           or the maximum number of characters. *)
                                        fun findEnd lastBreak i =
                                            if i >= textLength then (textLength, textLength)
                                            else if i-p > nCols
                                            then
                                                (
                                                case lastBreak of
                                                    NONE => (* No breaks on the line - break just before here. *)
                                                        (i-1, i-1)
                                                |    SOME b => b (* Break at the last break. *)
                                                )
                                            else if i < textLength - 1 andalso
                                                    String.sub(printWhat, i) = #"\r" andalso
                                                    String.sub(printWhat, i+1) = #"\n"
                                            then (* End of line - stop here. *)
                                                (i, i+2)
                                            else if Char.isSpace(String.sub(printWhat, i))
                                            then (* Remember this. *)
                                                findEnd (SOME(i, i+1)) (i+1)
                                                (* Actually tabs need to be handled more carefully. *)
                                            else findEnd lastBreak (i+1)

                                        val (endLine, nextLine) = findEnd NONE p
                                        val thisLine =
                                            if p >= textLength
                                            then ""
                                            else String.substring(printWhat, p, endLine-p)
                                    in
                                        if printThisPage
                                        then
                                            (
                                            TabbedTextOut(hdc, {x=0, y= lineNo * #height textMetric},
                                                thisLine, [], 0);
                                            ()
                                            )
                                        else ();
                                        outputLines (lineNo+1) nextLine
                                    end
                                val nextPage =
                                    if printThisPage
                                    then
                                        let
                                            val _ = StartPage hdc;
                                            (* Fill the page with white. *)
                                            val _ = SetBkColor(hdc, white);
                                            val _ = SetTextColor(hdc, black);
                                            val _ = ExtTextOut(hdc, {x=0, y=0}, [ETO_OPAQUE], SOME pageRect, "", []);
                                            (* Print the text. *)
                                            val next = outputLines 0 index
                                        in
                                            EndPage hdc;
                                            next
                                        end
                                    else (* Format the page but don't print it. *) outputLines 0 index
                            in
                                if nextPage >= size printWhat
                                then ()
                                else printPage (pno+1) nextPage
                            end

                            val _: unit = printPage 1 0
                        in
                            EndDoc hdc;
                            (* Restore the original font and delete the new one. *)
                            SelectObject(hdc, oldFont);
                            DeleteObject(hFont);
                            DeleteDC hdc; (* Now delete the device context. *)
                            (LRESINT 0, SOME{edit=edit, devMode=devMode, devNames=devNames,
                                          fileName=fileName})
                        end
                            (* If any of the functions failed simply delete the device
                               context and return the original state. *)
                            handle (exn as OS.SysErr _) => (
                                print (exnName exn); AbortDoc hdc; DeleteDC hdc; (LRESINT 0, state)))
                    |    _ => (LRESINT 0, state)
                end

                else if wId = menuAbout
                then (aboutmlEdit hw; (LRESINT 0, state))

                else (DefWindowProc(hw, msg), state)

        |  FINDMSGSTRING{flags, findWhat, ...} =>
            if FindReplaceFlags.anySet(flags, FindReplaceFlags.FR_DIALOGTERM)
            then (* The "find" box is going away. *)
                (
                    SetFocus(SOME edit);
                    (LRESINT 0, state)
                )
            else if FindReplaceFlags.anySet(flags, FindReplaceFlags.FR_FINDNEXT)
            then (* The Find Next button has been pressed. *)
            let
                (* Get the whole of the text - not very efficient. *)
                val text = GetWindowText edit
                val startPos = ref 0 and endPos = ref 0
                (* Get the starting position. *)
                val _ = SendMessage(edit, EM_GETSEL{startPos=startPos, endPos=endPos})

                val isDown = FindReplaceFlags.anySet(flags, FindReplaceFlags.FR_DOWN)
                (* Get the starting position for the search. *)
                val startPos = if isDown then !endPos else !startPos - 1

                val findLen = size findWhat
                (* Get the options. *)
                local
                    val toLower = String.map Char.toLower
                in
                    val doMatch: string * string -> bool =
                        if FindReplaceFlags.anySet(flags, FindReplaceFlags.FR_MATCHCASE)
                        then op =
                        else fn (s1, s2) => toLower s1 = toLower s2
                end

                fun doFind p =
                let
                    val isMatch =
                        p >= 0 andalso size text - p >= size findWhat andalso
                            doMatch(String.substring(text, p, findLen), findWhat)
                in
                    if isMatch then p
                    else if isDown
                    then if p = size text then p (* Finish *) else doFind(p+1)
                    else (* Find up *) if p = 0 then ~1 (* Finish *) else doFind(p-1)
                end
                val foundAt = doFind startPos
            in
                if foundAt >= 0 andalso foundAt + findLen < size text
                then
                    (
                    SendMessage(edit, EM_SETSEL{startPos=foundAt, endPos=foundAt + findLen});
                    SendMessage(edit, EM_SCROLLCARET);
                    ()
                    )
                else MessageBeep(MessageBoxStyle.fromWord 0wxFFFFFFFF);
                (LRESINT 0, state)
            end
            else (DefWindowProc(hw, msg), state)

        |    _ => (DefWindowProc(hw, msg), state)

    (* If this document has been modified we want to ask before quitting or
       opening a new document. *)
    and checkForSave(hw, edit, fileName) =
        case SendMessage(edit, EM_GETMODIFY) of
            LRESINT 0 => true (* Unmodified - continue. *)
        |    _ => 
            let
                val res =
                    MessageBox(SOME hw, "Save document?", "Confirm",
                               MessageBoxStyle.MB_YESNOCANCEL)
            in
                if res = IDYES
                then if fileName = ""
                then saveAsDocument(hw, edit) <> NONE
                else saveDocument(hw, fileName, edit)
                else if res = IDNO
                then true (* Continue anyway. *)
                else false (* Cancel - don't exit or open. *)
            end

    and saveDocument(hw, fileName, edit) =
    (* Write the document to the given file name. *)
        let
            (* Write the file as binary.  That way we don't need to
               convert CRNL to NL before writing. *)
            val f = BinIO.openOut fileName
            val s = GetWindowText edit
        in
            BinIO.output(f, Byte.stringToBytes s);
            BinIO.closeOut f;
            (* Document is now unmodified. *)
            SendMessage(edit, EM_SETMODIFY{modified=false});
            true (* Succeeded. *)
        end handle exn =>
            (MessageBox(SOME hw,
                concat["Unable to save to - ", fileName, "\n"(*, exnMessage exn*)],
                "Open failure", MessageBoxStyle.MB_OK);
             false)

    and saveAsDocument(hw, edit) =
    (* Ask for the file name before trying to save. *)
        let
            val on = {
                owner = SOME hw,
                template = TemplateDefault,
                filter =
                    [("Text Files (*.txt)", "*.txt"),
                     ("ML Files (*.sml)", "*.sml"),
                     ("All Files (*.*)", "*.*")],
                customFilter = NONE,
                filterIndex = 1,
                file = "",
                maxFile = 1000,
                fileTitle = "",
                initialDir = NONE,
                title = NONE,
                flags = OpenFileFlags.flags[],
                defExt = NONE
            }
        in
            case GetSaveFileName on of
                NONE => NONE
            |    SOME {file, filterIndex, fileTitle, ...} =>
                let
                    (* If the user typed a file name without an extension use
                       the extension from the appropriate filter. *)
                    val suffix =
                        case filterIndex of
                            1 => ".txt"
                        |    2 => ".sml"
                        |    _ => ""
                    val fileName =
                        if Char.contains fileTitle #"."
                        then file else file ^ suffix
                in
                    if saveDocument(hw, fileName, edit)
                    then SOME file (* Return the selected name. *)
                    else NONE
                end
        end

    and aboutmlEdit hw =
    (* Called when the user selects "About..." from the help menu. *)
    let
        (* Dialogue template containing three items: an OK button, a static picture and
           a piece of text. *)
        val pictureId = 1000 (* Could use any number here. *)
        open Static.Style
        val template =
            {x = 0, y = 0, cx = 210, cy = 94, font = SOME (8, "MS Sans Serif"), menu = NONE,
             class = NONE,title = "About mlEdit", extendedStyle = 0,
             style = flags[WS_POPUP, WS_CAPTION],
             items =
              [{x = 73, y = 62, cx = 50, cy = 14, id = 1,
                class = DLG_BUTTON (flags[WS_CHILD, WS_VISIBLE, WS_TABSTOP]),
                title = DLG_TITLESTRING "OK", creationData = NONE, extendedStyle = 0},
               {x = 7, y = 7, cx = 32, cy = 32, id = pictureId,
                class = DLG_STATIC (flags[WS_CHILD, WS_VISIBLE, SS_ICON]),
                title = DLG_TITLESTRING "", creationData = NONE, extendedStyle = 0},
               {x = 15, y = 39, cx = 180, cy = 21, id = 65535,
                class = DLG_STATIC (flags[WS_CHILD, WS_VISIBLE, WS_GROUP]),
                title =
                    DLG_TITLESTRING
                       "mlEdit - An example of Windows programming in Poly/ML\
                       \\nCopyright David C.J. Matthews 2001-7",
                creationData = NONE,  extendedStyle = 0}] }

        (* Dialogue procedure. *)
        fun dlgProc(dial, WM_INITDIALOG _, ()) =
            (
                (* Send a message to the picture control to set it to this icon. *)
                SendMessage(GetDlgItem(dial, pictureId), STM_SETICON{icon=polyIcon});
                (LRESINT 1, ())
            )

        |    dlgProc(dial, WM_COMMAND{notifyCode = 0, wId=1 (* OK button *), ...}, ()) =
                (* When the OK button is pressed we end the dialogue. *)
                (EndDialog(dial, 1); (LRESINT 1, ()) )

        |    dlgProc _ = (LRESINT 0, ())

    in
        DialogBoxIndirect(app, template, hw, dlgProc, ());
        ()
    end

    val className = "mlEditWindowClass"
    (* Register a class for the top-level window.  Use the Poly icon from the application. *)
    val myWindowClass = RegisterClassEx{style = Class.Style.flags[],
        wndProc = wndProc, hInstance = app,
        hIcon = SOME polyIcon, hCursor = NONE, hbrBackGround = NONE, menuName = NONE,
        className = className, hIconSm = NONE};
    
    
    val w = CreateWindow{class = myWindowClass, name = "mlEdit", style = Window.Style.WS_OVERLAPPEDWINDOW,
        x  = CW_USEDEFAULT, y = CW_USEDEFAULT, height = CW_USEDEFAULT, width = CW_USEDEFAULT,
        relation = PopupWindow menu,
        instance = app, init = NONE};
in
    ShowWindow(w, SW_SHOW);
    SetForegroundWindow w;
    
    RunApplication();
    (* Must unregister the class before returning otherwise RegisterClass will
       fail if we call mlEdit again. *)
    UnregisterClass(className, app)
end;
