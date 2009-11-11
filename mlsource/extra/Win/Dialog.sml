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

(*
Dialogue boxes and operations on them.
*)
structure Dialog:
sig
    type HWND and HINSTANCE 
    datatype
      DLGCLASSES =
          DLG_CLASS of string * Window.Style.flags
        | DLG_BUTTON of Button.Style.flags
        | DLG_COMBOBOX of Combobox.Style.flags
        | DLG_EDIT of Edit.Style.flags
        | DLG_LISTBOX of Listbox.Style.flags
        | DLG_SCROLLBAR of Scrollbar.Style.flags
        | DLG_STATIC of Static.Style.flags

    datatype DLGTITLE = DLG_TITLERESOURCE of int | DLG_TITLESTRING of string
 
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
        and DS_3DLOOK: flags and DS_ABSALIGN: flags and DS_CENTER: flags and DS_CENTERMOUSE: flags
        and DS_CONTEXTHELP: flags and DS_CONTROL: flags and DS_FIXEDSYS: flags
        and DS_LOCALEDIT: flags and DS_MODALFRAME: flags and DS_NOFAILCREATE: flags
        and DS_NOIDLEMSG: flags and DS_SETFONT: flags and DS_SETFOREGROUND: flags
        and DS_SYSMODAL: flags
    end

    type DLGITEMTEMPLATE =
        { extendedStyle: int,
          x: int,
          y: int,
          cx : int,
          cy: int,
          id: int,
          class: DLGCLASSES,
          title: DLGTITLE,
          creationData: Word8Vector.vector option
        }
    
    type DLGTEMPLATE =
        { style: Style.flags,
          extendedStyle: int,
          x : int,
          y: int,
          cx: int,
          cy: int,
          menu: Resource.RESID option,
          class: Resource.RESID option,
          title: string,
          font: (int * string) option,
          items: DLGITEMTEMPLATE list
        }

    
    val DialogBox :
        HINSTANCE * Resource.RESID * HWND *
        (HWND * Message.Message * 'a -> Message.LRESULT * 'a) * 'a -> int
    val DialogBoxIndirect: HINSTANCE * DLGTEMPLATE * HWND *
        (HWND * Message.Message * 'a -> Message.LRESULT * 'a) * 'a -> int
    val CreateDialog : HINSTANCE * Resource.RESID * HWND *
        (HWND * Message.Message * 'a -> Message.LRESULT * 'a) * 'a -> HWND
    val CreateDialogIndirect: HINSTANCE * DLGTEMPLATE * HWND *
        (HWND * Message.Message * 'a -> Message.LRESULT * 'a) * 'a -> HWND

    val GetDialogBaseUnits : unit -> {horizontal: int, vertical: int}
    
    val GetDlgCtrlID: HWND -> int
    and GetDlgItem: HWND * int -> HWND
    and GetDlgItemText: HWND * int -> string
    and IsDialogMessage: HWND * Message.MSG -> bool
    and EndDialog: HWND * int -> unit

(* MessageBox and MessageBeep are in the MessageBox structure. *)
(*
CreateDialogIndirectParam  
CreateDialogParam  
DefDlgProc  - Used to create custom dialogues
DialogBoxIndirectParam  
DialogBoxParam  
DialogProc  
GetDlgItemInt  - Ignore - probably better done with Int.toString
SetDlgItemInt  - ditto
GetNextDlgGroupItem  
GetNextDlgTabItem  
MapDialogRect  
MessageBoxEx  
SendDlgItemMessage  
SetDlgItemText  
MessageBoxIndirect  
*)


    val compileTemplate : DLGTEMPLATE -> Word8Vector.vector
    val decompileTemplate : Word8Vector.vector -> DLGTEMPLATE
end =
struct
    local
        open CInterface
        open Base
        open Globals
        open Window
        open Resource

        fun user name = load_sym (load_lib "user32.dll") name
        and kernel name = load_sym (load_lib "kernel32.dll") name

        fun checkWindow c = (checkResult(not(isHNull c)); c)

        (* Dialogue procedures never call DefWindowProc. *)
        fun dlgProcRes (lres, state) = (lres, state)
        
        val DLGPROC = PASCALFUNCTION4 (HWND, INT, POINTER, POINTER) POINTER
    in
        type HWND = HWND and HINSTANCE = HINSTANCE

        datatype DLGCLASSES =
            DLG_CLASS of string * Window.Style.flags (* Named window class. *)
        |   DLG_BUTTON of Button.Style.flags
        |   DLG_EDIT of Edit.Style.flags
        |   DLG_STATIC of Static.Style.flags
        |   DLG_LISTBOX of Listbox.Style.flags
        |   DLG_SCROLLBAR of Scrollbar.Style.flags
        |   DLG_COMBOBOX of Combobox.Style.flags
    
        datatype DLGTITLE = DLG_TITLESTRING of string | DLG_TITLERESOURCE of int
    
        structure Style =
        struct
            open Window.Style (* Include all the windows styles. *)

            val DS_ABSALIGN: flags          = fromWord 0wx0001
            val DS_SYSMODAL: flags          = fromWord 0wx0002
            val DS_LOCALEDIT: flags         = fromWord 0wx0020
            val DS_SETFONT: flags           = fromWord 0wx0040
            val DS_MODALFRAME: flags        = fromWord 0wx0080
            val DS_NOIDLEMSG: flags         = fromWord 0wx0100
            val DS_SETFOREGROUND: flags     = fromWord 0wx0200
            val DS_3DLOOK: flags            = fromWord 0wx0004
            val DS_FIXEDSYS: flags          = fromWord 0wx0008
            val DS_NOFAILCREATE: flags      = fromWord 0wx0010
            val DS_CONTROL: flags           = fromWord 0wx0400
            val DS_CENTER: flags            = fromWord 0wx0800
            val DS_CENTERMOUSE: flags       = fromWord 0wx1000
            val DS_CONTEXTHELP: flags       = fromWord 0wx2000
    
            val all = flags[Window.Style.all, DS_ABSALIGN, DS_SYSMODAL, DS_LOCALEDIT, DS_SETFONT,
                            DS_MODALFRAME, DS_NOIDLEMSG, DS_SETFOREGROUND, DS_3DLOOK, DS_FIXEDSYS,
                            DS_NOFAILCREATE, DS_CONTROL, DS_CENTER, DS_CENTERMOUSE, DS_CONTEXTHELP]
    
            val intersect =
                List.foldl (fn (a, b) => fromWord(SysWord.andb(toWord a, toWord b))) all
        end
    
        type DLGITEMTEMPLATE =
            { extendedStyle: int,
              x: int,
              y: int,
              cx : int,
              cy: int,
              id: int,
              class: DLGCLASSES,
              title: DLGTITLE,
              creationData: Word8Vector.vector option
            }
        
        type DLGTEMPLATE =
            { style: Style.flags,
              extendedStyle: int,
              x : int,
              y: int,
              cx: int,
              cy: int,
              menu: Resource.RESID option,
              class: Resource.RESID option,
              title: string,
              font: (int * string) option,
              items: DLGITEMTEMPLATE list
            }

        (* Convert between the data structures and the templates. *)
        (* TODO: This only deals with the basic templates not the extended
           versions. *)
        fun decompileTemplate (w: Word8Vector.vector): DLGTEMPLATE =
        let
            val ptr = ref 0
            val isExtended = PackWord32Little.subVec(w, 0) = 0wxFFFF0001
            val _ = if isExtended then raise Fail "Extended templates not implemented" else ();
    
            val style = PackWord32Little.subVec(w, !ptr div 4)
            val _ = ptr := !ptr + 4;
            val exStyle = LargeWord.toInt(PackWord32Little.subVec(w, !ptr div 4))
            val _ = ptr := !ptr + 4;
            val cdit = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
            val _ = ptr := !ptr + 2;
            val x = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
            val _ = ptr := !ptr + 2;
            val y = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
            val _ = ptr := !ptr + 2;
            val cx = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
            val _ = ptr := !ptr + 2;
            val cy = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
            val _ = ptr := !ptr + 2;

            (* Extract a null-terminated Unicode string and advance ptr beyond it. *)
            fun getString () =
            let
                val start = !ptr
                fun advance () =
                let
                    val next = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
                in
                    ptr := !ptr + 2;
                    if next = 0 then () else advance()
                end
            in
                advance();
                unicodeToString(Word8VectorSlice.vector(Word8VectorSlice.slice(w, start, SOME(!ptr-start-2))))
            end

            fun ffffOrString(): Resource.RESID =
            let
                val next = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
            in
                if next = 0xffff
                then ( (* Resource identifier. *)
                    ptr := !ptr + 4;
                    Resource.IdAsInt(LargeWord.toInt(PackWord16Little.subVec(w, (!ptr-2) div 2)))
                )
                else (* Resource name. *)
                    Resource.IdAsString(getString())
            end

            (* Menu. *)
            val menu =
                case ffffOrString() of
                    Resource.IdAsString "" => NONE
                |   r => SOME r

            (* Class. *)
            val class =
                case ffffOrString() of
                    Resource.IdAsString "" => NONE
                |   r => SOME r

            (* Title - null terminated Unicode string. *)
            val title = getString()
            (* Font - only if DS_SETFONT included in the style. *)
            val font =
                if Style.anySet(Style.fromWord style, Style.DS_SETFONT)
                then
                let
                    val size = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
                    val _ = ptr := !ptr + 2
                    val name = getString()
                in
                    SOME(size, name)
                end
                else NONE
            
            (* Items. *)
            fun processItem n : DLGITEMTEMPLATE =
            let
                (* Must be aligned onto a DWORD boundary. *)
                val _ = while !ptr mod 4 <> 0 do ptr := !ptr + 1;

                val style = PackWord32Little.subVec(w, !ptr div 4)
                val _ = ptr := !ptr + 4;
                val exStyle = LargeWord.toInt(PackWord32Little.subVec(w, !ptr div 4))
                val _ = ptr := !ptr + 4;
                val x = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
                val _ = ptr := !ptr + 2;
                val y = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
                val _ = ptr := !ptr + 2;
                val cx = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
                val _ = ptr := !ptr + 2;
                val cy = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
                val _ = ptr := !ptr + 2;
                val id = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
                val _ = ptr := !ptr + 2;

                val class =
                    case ffffOrString() of
                        Resource.IdAsString s => DLG_CLASS (s, Window.Style.fromWord style)
                    |   Resource.IdAsInt 0x0080 => DLG_BUTTON (Button.Style.fromWord style)
                    |   Resource.IdAsInt 0x0081 => DLG_EDIT (Edit.Style.fromWord style)
                    |   Resource.IdAsInt 0x0082 => DLG_STATIC (Static.Style.fromWord style)
                    |   Resource.IdAsInt 0x0083 => DLG_LISTBOX (Listbox.Style.fromWord style)
                    |   Resource.IdAsInt 0x0084 => DLG_SCROLLBAR (Scrollbar.Style.fromWord style)
                    |   Resource.IdAsInt 0x0085 => DLG_COMBOBOX (Combobox.Style.fromWord style)
                    |   _ => raise Fail "Unknown dialog type"

                val title = 
                    case ffffOrString() of
                        Resource.IdAsString s => DLG_TITLESTRING s
                    |   Resource.IdAsInt i => DLG_TITLERESOURCE i

                val creation =
                let
                    val length = LargeWord.toInt(PackWord16Little.subVec(w, !ptr div 2))
                    val _ = ptr := !ptr + 2;
                    val start = !ptr
                    val _ = ptr := !ptr + length
                in
                    if length = 0
                    then NONE
                    else SOME(Word8VectorSlice.vector(Word8VectorSlice.slice(w, start, SOME length)))
                end
            in
            {
                extendedStyle = exStyle,
                x = x,
                y = y,
                cx = cx,
                cy = cy,
                id = id,
                class = class,
                title = title,
                creationData = creation
            }
            end
        in
            { style = Style.fromWord style,
              extendedStyle = exStyle,
              x = x,
              y = y,
              cx = cx,
              cy = cy,
              menu = menu,
              class = class,
              title = title,
              font = font,
              items = List.tabulate(cdit, processItem)
            }
        end;

        (* Generate a dialogue template in memory. *)
        fun compileTemplate (t: DLGTEMPLATE) =
        let
            val basis = Word8Array.array (18, 0w0)
            val nullString = Word8Vector.tabulate(2, fn _ => 0w0)
            (* Force DS_SETFONT in the style according to whether we have a font specified. *)
            val style =
                if #font t = NONE
                then Style.clear(Style.DS_SETFONT, #style t)
                else Style.flags[#style t, Style.DS_SETFONT]
            val _ = PackWord32Little.update(basis, 0, Style.toWord style);
            val _ = PackWord32Little.update(basis, 1, LargeWord.fromInt(#extendedStyle t));
            val _ = PackWord16Little.update(basis, 4, LargeWord.fromInt(List.length(#items t)));
            val _ = PackWord16Little.update(basis, 5, LargeWord.fromInt(#x t));
            val _ = PackWord16Little.update(basis, 6, LargeWord.fromInt(#y t));
            val _ = PackWord16Little.update(basis, 7, LargeWord.fromInt(#cx t));
            val _ = PackWord16Little.update(basis, 8, LargeWord.fromInt(#cy t));

            fun sixteenBit i = Word8Vector.fromList[Word8.fromInt i, Word8.fromInt(i div 256)]

            fun unicodeString s = Word8Vector.concat[stringToUnicode s, nullString]

            fun resOrString (Resource.IdAsString s) = unicodeString s
              | resOrString (Resource.IdAsInt i) =
                    Word8Vector.fromList
                            [0wxff, 0wxff, Word8.fromInt i, Word8.fromInt(i div 256)]
            val menu =
                case #menu t of
                    NONE => nullString
                |   SOME r => resOrString r

            val class = 
                case #class t of
                    NONE => nullString
                |   SOME r => resOrString r

            val title = unicodeString(#title t)
            val font =
                case #font t of
                    SOME (size, name) =>
                        [Word8Vector.fromList
                            [Word8.fromInt size, Word8.fromInt(size div 256)],
                         stringToUnicode name, nullString]
                        
                |   NONE => []

            fun compileItems [] = []
            |   compileItems((t: DLGITEMTEMPLATE) :: rest) =
            let
                val basis = Word8Array.array(18, 0w0)
                val (style, class) =
                    case #class t of
                      DLG_CLASS(c, s)   => (Window.Style.toWord s, Resource.IdAsString c)
                    | DLG_BUTTON s      => (Button.Style.toWord s, Resource.IdAsInt 0x80)
                    | DLG_COMBOBOX s    => (Combobox.Style.toWord s, Resource.IdAsInt 0x85)
                    | DLG_EDIT s        => (Edit.Style.toWord s, Resource.IdAsInt 0x81)
                    | DLG_LISTBOX s     => (Listbox.Style.toWord s, Resource.IdAsInt 0x83)
                    | DLG_SCROLLBAR s   => (Scrollbar.Style.toWord s, Resource.IdAsInt 0x84)
                    | DLG_STATIC s      => (Static.Style.toWord s, Resource.IdAsInt 0x82)

                val _ = PackWord32Little.update(basis, 0, style);
                val _ = PackWord32Little.update(basis, 1, LargeWord.fromInt(#extendedStyle t));
                val _ = PackWord16Little.update(basis, 4, LargeWord.fromInt(#x t));
                val _ = PackWord16Little.update(basis, 5, LargeWord.fromInt(#y t));
                val _ = PackWord16Little.update(basis, 6, LargeWord.fromInt(#cx t));
                val _ = PackWord16Little.update(basis, 7, LargeWord.fromInt(#cy t));
                val _ = PackWord16Little.update(basis, 8, LargeWord.fromInt(#id t));
                val title =
                    resOrString(
                        case #title t of
                            DLG_TITLESTRING s => Resource.IdAsString s
                        |   DLG_TITLERESOURCE i => Resource.IdAsInt i) 

                val creation =
                    case #creationData t of
                        NONE => [nullString]
                    |   SOME r => [r, nullString]
                val vec = 
                    Word8Vector.concat
                        (Word8ArraySlice.vector(Word8ArraySlice.full basis) ::
                         resOrString class :: title :: creation)
                val rounding = Word8Vector.length vec mod 4
            in
                (* Must align onto a 4-byte boundary except for the last. *)
                (if rounding = 0 orelse rest = nil then vec
                 else Word8Vector.concat[vec, Word8Vector.tabulate(4-rounding, fn _ => 0w0)]) ::
                    compileItems rest
            end

            val header = 
                Word8Vector.concat
                    (Word8ArraySlice.vector(Word8ArraySlice.full basis) :: menu :: class :: title :: font)
            val rounding = Word8Vector.length header mod 4
            val alignment = Word8Vector.tabulate(4-rounding, fn _ => 0w0)
        in
            Word8Vector.concat(header :: alignment :: compileItems (#items t))
        end

        (* CreateDialogIndirect: Create a modeless dialogue using a resource. *)
        local
            val sysCreateDialog =
                call5 (user "CreateDialogParamA") (HINSTANCE, RESID, HWND, DLGPROC, INT) HWND
        in
            fun CreateDialog (hInst, lpTemplate, hWndParent, dialogueProc, init) =
            let
                val _ = Message.setCallback(dlgProcRes o dialogueProc, init);
                val res = checkWindow
                    (sysCreateDialog(hInst, lpTemplate, hWndParent, Message.mainCallbackFunction, 0))
            in
                (* Add this to the modeless dialogue list so that keyboard
                   operations will work. *)
                Message.addModelessDialogue(res, toCint 0);
                res
            end
        end

        (* CreateDialogIndirect: Create a modeless dialogue from a template. *)
        local
            val sysCreateDialogIndirect =
                call5 (user "CreateDialogIndirectParamA") (HINSTANCE, POINTER, HWND, DLGPROC, INT) HWND
        in
            fun CreateDialogIndirect (hInst, template, hWndParent, dialogueProc, init) =
            let
                val _ = Message.setCallback(dlgProcRes o dialogueProc, init);
                (* Compile the template and copy it to C memory. *)
                val compiled = compileTemplate template
                val size = Word8Vector.length compiled
                val templ = alloc size Cchar
                fun copyToBuf(i, v): unit =
                    assign Cchar (offset i Cchar templ) (toCint(Word8.toInt v))
                val _ = Word8Vector.appi copyToBuf compiled
                val res = checkWindow
                    (sysCreateDialogIndirect(hInst, address templ, hWndParent, Message.mainCallbackFunction, 0))
            in
                (* Add this to the modeless dialogue list so that keyboard
                   operations will work. *)
                Message.addModelessDialogue(res, toCint 0);
                res
            end
        end

        (* DialogBox: create a dialogue using a resource. *)
        local
            val sysDialogBox =
                call5 (user "DialogBoxParamA") (HINSTANCE, RESID, HWND, DLGPROC, INT) INT
        in
            fun DialogBox (hInst, lpTemplate, hWndParent, dialogueProc, init) =
            let
                (* We can use the normal window procedure as a dialogue proc. *)
                val _ = Message.setCallback(dlgProcRes o dialogueProc, init);
                val result = sysDialogBox(hInst, lpTemplate, hWndParent, Message.mainCallbackFunction, 0)
            in
                (* How do we remove the callback?  Look for the last message? *)
                result
        end
        end

        (* DialogBoxIndirect: create a dialogue using a template. *)
        local
            val sysDialogBoxIndirect =
                call5 (user "DialogBoxIndirectParamA") (HINSTANCE, POINTER, HWND, DLGPROC, INT) INT
        in
            fun DialogBoxIndirect (hInst, template, hWndParent, dialogueProc, init) =
            let
                val _ = Message.setCallback(dlgProcRes o dialogueProc, init);
                (* Compile the template and copy it to C memory. *)
                val compiled = compileTemplate template
                val size = Word8Vector.length compiled
                val templ = alloc size Cchar
                fun copyToBuf(i, v): unit =
                    assign Cchar (offset i Cchar templ) (toCint(Word8.toInt v))
                val _ = Word8Vector.appi copyToBuf compiled
            in
                sysDialogBoxIndirect(hInst, address templ, hWndParent, Message.mainCallbackFunction, 0)
            end
        end

        (* Get average size of system font. *)
        fun GetDialogBaseUnits() : {horizontal: int, vertical: int} =
        let
            val base = call0 (user "GetDialogBaseUnits") () UINT ()
        in
            {horizontal = LOWORD base, vertical = HIWORD base}
        end
    
        val GetDlgCtrlID = call1 (user "GetDlgCtrlID") HWND INT
        and GetDlgItem   = call2 (user "GetDlgItem") (HWND, INT) HWND
    
        val GetDlgItemText = Window.GetWindowText o GetDlgItem

        val IsDialogMessage = call2 (user "IsDialogMessage") (HWND, Message.LPMSG) BOOL

        val EndDialog = call2 (user "EndDialog") (HWND, INT) (SUCCESSSTATE "EndDialog")
    end
end;
