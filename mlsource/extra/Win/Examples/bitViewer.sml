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
(* Example bitmap viewer. *)
fun bitViewer() =
let
    open Window Message Class Clipboard Menu Bitmap DeviceContext Brush Metafile
    open Font Color Painting Globals

    val app = ApplicationInstance()

    (* Identifiers for the menu items. *)
    val menuCopy = 1
    and menuPaste = 2

    local
        val editMenu =
            let
                val editMenu = CreateMenu();
            in
                AppendMenu(editMenu, [], MenuId menuCopy, MFT_STRING "&Copy");
                AppendMenu(editMenu, [], MenuId menuPaste, MFT_STRING "&Paste");
                editMenu
            end;

    in
        (* Create the main menu and append the sub-menu. *)
        val menu = CreateMenu();
        val _ = AppendMenu(menu, [], MenuHandle editMenu, MFT_STRING "&Edit")
    end;

    (* Window procedure for the window. *)
    fun wndProc(w: HWND, WM_CREATE _, NONE) =
        let
            (* Create a sub-window to display a bitmap. *)
            val static = CreateWindow{class = Static, name = "",
                                style = Static.Style.flags[Static.Style.WS_CHILD, Static.Style.WS_VISIBLE, Static.Style.SS_BITMAP],
                                x  = CW_USEDEFAULT, y = CW_USEDEFAULT,
                                height = CW_USEDEFAULT, width = CW_USEDEFAULT,
                                relation = ChildWindow{parent=w, id=99 (* Not used *)},
                                instance = app, init = ()}
        in
            (LRESINT 0, SOME static) (* Return it as the "state". *)
        end

    |   wndProc(w: HWND, WM_PAINT, state as SOME static) =
            (* WM_PAINT is sent to the window when at least some of it needs to be redrawn. *)
        let
            (* Fill this window with grey.  We use this to fill all the area that
               isn't occupied by the subwindow containing the bitmap.
               A simpler way of doing this is to define a background colour for
               the class in which case we don't have to process this message at all. *)
            val (hdc, ps) = BeginPaint w
            val grey = RGB{red=128, blue=128, green=128}
            val _ = SetBkColor(hdc, grey);
            (* ExtTextOut with the ETO_OPAQUE is a common way of filling an area with
               a single colour. *)
            val _ = ExtTextOut(hdc, {x=0, y=0}, [ETO_OPAQUE], SOME(#paint ps), "", []);
        in
            EndPaint(w, ps);
            (LRESINT 0, state)
        end

    |   wndProc(w: HWND, WM_SIZE {width, height, ...}, state as SOME static) =
            (* The main window has been resized.  Recentre the child window. *)
        let
            val subRect = GetClientRect static  
        in
            MoveWindow{hWnd = static, x = (width - #right subRect) div 2,
                       y = (height - #bottom subRect) div 2,
                       height = #bottom subRect, width = #right subRect, repaint = true};
            (LRESINT 0, state)
        end

    |   wndProc(w: HWND, msg as WM_COMMAND{notifyCode = 0, wId = 1 (* menuCopy*), control},
                state as SOME static) =
        (* WM_COMMAND messages are sent when a menu item is pulled down.  wId is the
           value we set as the ID when we created the menu. *)
        (* Copy the bitmap to the window as a device-independent bitmap.  We could
           equally just copy the bitmap handle and let the clipboard do the work. *)
        let
            val LRESHANDLE (bitMap: HBITMAP) =
                SendMessage(static, STM_GETIMAGE{imageType=IMAGE_BITMAP})
        in
            if isHNull bitMap
            then (DefWindowProc(w, msg), state)
            else (
                OpenClipboard(SOME w);
                let
                    val _ = EmptyClipboard(); (* Become owner. *)
                    val winDC = GetDC static
                    (* The first call returns the header of the bitmap that will be
                       created.  We can use all the defaults in it but we need to
                       know the height which is the number of scan lines. *)
                    val dibHdr as {height, ...} =
                        getBitmapInfoHdr(GetDIBits(winDC, bitMap, 0, 0, NONE))
                    val dib = GetDIBits(winDC, bitMap, 0, height, SOME dibHdr)
                    val _ = ReleaseDC(static, winDC)
                in
                    SetClipboardData(CH_DIB dib)
                end (* Make sure the clipboard is closed if anything goes wrong. *)
                handle exn => (CloseClipboard(); raise exn);
                CloseClipboard();
                (LRESINT 0, state)
                )
        end

    |  wndProc(w: HWND, msg as WM_COMMAND{notifyCode = 0, wId = 2 (* menuPaste*), control},
               state as SOME static) =
        (
        case GetPriorityClipboardFormat[CF_DIB, CF_BITMAP, CF_ENHMETAFILE] of
            (* Some drawing programs paste metafile information rather than bitmaps. *)
            SOME CF_ENHMETAFILE =>
            (
                OpenClipboard(SOME w);

                let
                    val CH_ENHMETAFILE emh = GetClipboardData CF_ENHMETAFILE;
                    (* Get the bounding frame of this metafile.  This gives us the size of
                       the bitmap. *)
                    val hdr as {bounds = { right, left, top, bottom }, ... } = GetEnhMetaFileHeader emh
                    val winDC = GetDC static
                    (* In order to write to a new bitmap we first have to create a memory
                       device context and select a new bitmap into it. *)
                    val memDC = CreateCompatibleDC winDC
                    val emfSize: SIZE = {cx = right - left, cy = bottom - top}
                    val newbm = CreateCompatibleBitmap(winDC, right - left, bottom - top)
                    (* Select the bitmap into the memory DC to draw to it. *)
                    val oldBM = SelectObject(memDC, newbm);
                    (* Play the metafile *)
                    val _ = PlayEnhMetaFile(memDC, emh,
                        {top = 0, left = 0, right = right - left, bottom = bottom - top })
                    (* Deselect the bitmap by selecting in the original (most likely NULL). *)
                    val _ = SelectObject(memDC, oldBM);
                    val _ = DeleteDC memDC (* Get rid of the device context. *)
                    val _ = ReleaseDC(static, winDC)
                in
                    setBitmap(newbm, w, static)
                end
                (* Make sure the clipboard is closed if anything goes wrong. *)
                handle exn => (CloseClipboard(); raise exn);

                CloseClipboard();
                (LRESINT 0, state)
            )

        |   SOME CF_DIB =>
            (
                OpenClipboard(SOME w);

                let
                    val CH_DIB dib = GetClipboardData CF_DIB;
                    val winDC = GetDC static
                    val { width, height, ...} = getBitmapInfoHdr dib
                    (* Height could be negative for a top-down DIB. *)
                    val newbm = CreateCompatibleBitmap(winDC, width, abs height)
                    val _ = SetDIBits(winDC, newbm, 0, abs height, dib)
                    val _ = ReleaseDC(static, winDC)
                in
                    setBitmap(newbm, w, static)
                end
                (* Make sure the clipboard is closed if anything goes wrong. *)
                handle exn => (CloseClipboard(); raise exn);

                CloseClipboard();
                (LRESINT 0, state)
            )

        (* The clipboard synthesises DIBs from bitmaps so this code will never be
           executed.  It's included for information only. *)
        |   SOME CF_BITMAP =>
            (
                OpenClipboard(SOME w);
                let
                    val CH_BITMAP hb = GetClipboardData CF_BITMAP;
                    (* Get the size of the bitmap *)
                    val GO_Bitmap (bmp as {widthBytes, height, ...}) = GetObject hb
                    (* Create a copy. *)
                    val newb = CreateBitmapIndirect bmp
                    val bytes = widthBytes*height
                    val v = GetBitmapBits(hb, bytes)
                    val _  = SetBitmapBits(newb, v);
                in
                    setBitmap(newb, w, static)
                end
                (* Make sure the clipboard is closed if anything goes wrong. *)
                handle exn => (CloseClipboard(); raise exn);

                CloseClipboard();
                (LRESINT 0, state)
            )
        |   _ => (DefWindowProc(w, msg), state) (* Nothing we can use. *)
        )

    |  wndProc(w: HWND, msg as WM_NCDESTROY, state) =
        (* When the window is closed we send a QUIT message which exits from the application loop. *)
        (PostQuitMessage 0; (DefWindowProc(w, msg), state))

    |  wndProc(w: HWND, msg: Message, state) =
        (DefWindowProc(w, msg), state) (* Anything else. *)

    and setBitmap(newb: HBITMAP, main: HWND, static: HWND) =
    let
        (* Set this copy as the new image and get back the old one. *)
        val LRESHANDLE oldBM =
            SendMessage(static, STM_SETIMAGE{image=newb, imageType=IMAGE_BITMAP})
        (* Get the size of the main window and the subwindow. *)
        val mainRect = GetClientRect main
        and subRect = GetClientRect static  
    in
        (* If there was previously a bitmap we have to delete it. *)
        if isHNull oldBM
        then ()
        else DeleteObject oldBM;
        (* Centre the window. *)
        MoveWindow{hWnd = static, x = (#right mainRect - #right subRect) div 2,
                   y = (#bottom mainRect - #bottom subRect) div 2,
                   height = #bottom subRect, width = #right subRect, repaint = true}
    end

    (* Register a window class. *)
    val at = Class.RegisterClassEx{style = Class.Style.flags[],
        wndProc = wndProc,
        hInstance = app,
        hIcon = NONE, hCursor = NONE, hbrBackGround = NONE, menuName = NONE,
        className = "bitViewerClass", hIconSm = NONE};
    
    val w = CreateWindow{class = at, name = "bitViewer", style = Window.Style.WS_OVERLAPPEDWINDOW,
        x  = CW_USEDEFAULT, y = CW_USEDEFAULT, height = CW_USEDEFAULT, width = CW_USEDEFAULT,
        relation = PopupWindow menu,
        instance = app, init = NONE};

in

    ShowWindow(w, SW_SHOW);
    SetForegroundWindow w;
    
    RunApplication();
    UnregisterClass("bitViewerClass", app)
end;
