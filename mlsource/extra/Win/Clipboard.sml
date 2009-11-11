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

structure Clipboard :
  sig
    (* Clipboard formats.  I've added CF_NONE, CF_PRIVATE, CF_GDIOBJ and CF_REGISTERED *)
    datatype ClipboardFormat =
        CF_NONE | CF_TEXT | CF_BITMAP | CF_METAFILEPICT | CF_SYLK | CF_DIF | CF_TIFF |
        CF_OEMTEXT | CF_DIB | CF_PALETTE | CF_PENDATA | CF_RIFF | CF_WAVE | CF_UNICODETEXT |
        CF_ENHMETAFILE | CF_OWNERDISPLAY | CF_DSPTEXT | CF_DSPBITMAP | CF_DSPMETAFILEPICT |
        CF_DSPENHMETAFILE | CF_PRIVATE of int | CF_GDIOBJ of int | CF_REGISTERED of int |
        CF_HDROP | CF_LOCALE

    type HBITMAP and HPALETTE and HWND and HDROP

    datatype CLIPHANDLE =
        CH_NONE |
        CH_TEXT of string |
        CH_BITMAP of HBITMAP |
        CH_METAFILEPICT of Metafile.METAFILEPICT |
        CH_SYLK of Word8Vector.vector |
        CH_DIF of Word8Vector.vector |
        CH_TIFF of Word8Vector.vector |
        CH_OEMTEXT of string |
        CH_DIB of Word8Vector.vector |
        CH_PALETTE of HPALETTE |
        CH_PENDATA of Word8Vector.vector |
        CH_RIFF of Word8Vector.vector |
        CH_WAVE of Word8Vector.vector |
        CH_UNICODETEXT of Word8Vector.vector |
        CH_ENHMETAFILE of Metafile.HENHMETAFILE |
        CH_OWNERDISPLAY of Word8Vector.vector |
        CH_DSPTEXT of Word8Vector.vector |
        CH_DSPBITMAP of Word8Vector.vector |
        CH_DSPMETAFILEPICT of Word8Vector.vector |
        CH_DSPENHMETAFILE of Word8Vector.vector |
        CH_PRIVATE of int * Word8Vector.vector |
        CH_GDIOBJ of int * Word8Vector.vector |
        CH_REGISTERED of int * Word8Vector.vector |
        CH_HDROP of HDROP |
        CH_LOCALE of Word8Vector.vector

    val ChangeClipboardChain : HWND * HWND -> bool
    val CloseClipboard : unit -> unit
    val CountClipboardFormats : unit -> int
    val EmptyClipboard : unit -> unit
    val EnumClipboardFormats : ClipboardFormat -> ClipboardFormat
    val GetClipboardData : ClipboardFormat -> CLIPHANDLE
    val GetClipboardFormatName : ClipboardFormat -> string
    val GetClipboardOwner : unit -> HWND
    val GetClipboardViewer : unit -> HWND
    val GetOpenClipboardWindow : unit -> HWND
    val GetPriorityClipboardFormat : ClipboardFormat list -> ClipboardFormat option
    val IsClipboardFormatAvailable : ClipboardFormat -> bool
    val OpenClipboard : HWND option -> unit
    val RegisterClipboardFormat : string -> ClipboardFormat
    val SetClipboardData : CLIPHANDLE -> unit
    val SetClipboardViewer : HWND -> HWND
  end
 =
struct
    local
        open CInterface
        open Base
        val GMEM_SHARE = 0x2000
        and GMEM_MOVEABLE = 0x0002
        val GMEM_OPTS = IntInf.orb(GMEM_SHARE, GMEM_MOVEABLE)

        val (fromHG, toHG, _) = breakConversion HGLOBAL
        val (fromHPAL, toHPAL, _) = breakConversion HPALETTE
        val (fromHBIT, toHBIT, _) = breakConversion HBITMAP
        val (fromHENH, toHENH, _) = breakConversion HENHMETAFILE
        val (fromHDRP, toHDRP, _) = breakConversion HDROP
        val (fromMFP, toMFP, mfpStruct) = breakConversion GdiBase.METAFILEPICT
    in
        type HBITMAP = HBITMAP and HPALETTE = HPALETTE and HWND = HWND and HDROP = HDROP

        datatype ClipboardFormat = datatype ClipboardFormat

        (* The data is transferred to and from the clipboard in various formats.
           I've added this datatype to deal with them. *)
        datatype CLIPHANDLE =
            CH_NONE |
            CH_TEXT of string |
            CH_BITMAP of HBITMAP |
            CH_METAFILEPICT of Metafile.METAFILEPICT |
            CH_SYLK of Word8Vector.vector |
            CH_DIF of Word8Vector.vector |
            CH_TIFF of Word8Vector.vector |
            CH_OEMTEXT of string |
            CH_DIB of Word8Vector.vector |
            CH_PALETTE of HPALETTE |
            CH_PENDATA of Word8Vector.vector |
            CH_RIFF of Word8Vector.vector |
            CH_WAVE of Word8Vector.vector |
            CH_UNICODETEXT of Word8Vector.vector |
            CH_ENHMETAFILE of Metafile.HENHMETAFILE |
            CH_OWNERDISPLAY of Word8Vector.vector |
            CH_DSPTEXT of Word8Vector.vector |
            CH_DSPBITMAP of Word8Vector.vector |
            CH_DSPMETAFILEPICT of Word8Vector.vector |
            CH_DSPENHMETAFILE of Word8Vector.vector |
            CH_PRIVATE of int * Word8Vector.vector |
            CH_GDIOBJ of int * Word8Vector.vector |
            CH_REGISTERED of int * Word8Vector.vector |
            CH_HDROP of HDROP |
            CH_LOCALE of Word8Vector.vector

        val ChangeClipboardChain = call2 (user "ChangeClipboardChain") (HWND, HWND) BOOL
        and CloseClipboard = call0 (user "CloseClipboard") () (SUCCESSSTATE "CloseClipboard")
        and CountClipboardFormats = call0 (user "CountClipboardFormats") () INT
        and EmptyClipboard = call0 (user "EmptyClipboard") () (SUCCESSSTATE "EmptyClipboard")
        and EnumClipboardFormats = call1 (user "EnumClipboardFormats") (CLIPFORMAT) CLIPFORMAT
        and GetClipboardOwner = call0 (user "GetClipboardOwner") () HWND
        and GetClipboardViewer = call0 (user "GetClipboardViewer") () HWND
        and GetOpenClipboardWindow = call0 (user "GetOpenClipboardWindow") () HWND
        and IsClipboardFormatAvailable =
            call1 (user "IsClipboardFormatAvailable") (CLIPFORMAT) BOOL
        and OpenClipboard = call1 (user "OpenClipboard") (HWNDOPT) (SUCCESSSTATE "OpenClipboard")
        and RegisterClipboardFormat =
            CF_REGISTERED o call1 (user "RegisterClipboardFormat") (STRING) INT
        and SetClipboardViewer = call1 (user "SetClipboardViewer") (HWND) HWND

        (* SetClipboardData copies the data to the clipboard.  It is possible to pass
           NULL as the handle and instead process the WM_RENDERFORMAT message.  We
           don't support that. *)
        fun SetClipboardData(clip: CLIPHANDLE): unit =
        let
            (* Most clipboard data is passed in memory allocated using GlobalAlloc. *)
            fun globString (s: string):vol =
            let
                val hGlob = GlobalAlloc(GMEM_OPTS, size s)
                val mem = deref(GlobalLock hGlob)
            in
                fillCstring mem s;
                GlobalUnlock hGlob;
                toHG hGlob
            end
            and globMem (w: Word8Vector.vector): vol =
            let
                val length = Word8Vector.length w
                val hGlob = GlobalAlloc(GMEM_OPTS, length)
                val buf = deref(GlobalLock hGlob)
                fun copyToBuf (i, v): unit =
                    assign Cchar (offset i Cchar buf) (toCint(Word8.toInt v))
            in
                Word8Vector.appi copyToBuf w;
                GlobalUnlock hGlob;
                toHG hGlob
            end

            (* Convert the various data formats and get the format type to pass. *)
            val (cf, data) =
                case clip of
                    CH_NONE                 => raise Fail "SetClipboardData: No data"
                |   CH_TEXT t               => (CF_TEXT, globString t)
                |   CH_BITMAP b             => (CF_BITMAP, toHBIT b)
                |   CH_METAFILEPICT p =>
                    let
                        val mfp = toMFP p
                        val hGlob = GlobalAlloc(GMEM_OPTS, sizeof mfpStruct)
                    in
                        assign mfpStruct (deref(GlobalLock hGlob)) mfp;
                        GlobalUnlock hGlob;
                        (CF_METAFILEPICT, toHG hGlob)
                    end
                |   CH_SYLK m               => (CF_SYLK, globMem m)
                |   CH_DIF m                => (CF_DIF, globMem m)
                |   CH_TIFF m               => (CF_TIFF, globMem m)
                |   CH_OEMTEXT t            => (CF_OEMTEXT, globString t)
                |   CH_DIB m                => (CF_DIB, globMem m)
                |   CH_PALETTE p            => (CF_PALETTE, toHPAL p)
                |   CH_PENDATA m            => (CF_PENDATA, globMem m)
                |   CH_RIFF m               => (CF_RIFF, globMem m)
                |   CH_WAVE m               => (CF_WAVE, globMem m)
                |   CH_UNICODETEXT m        => (CF_UNICODETEXT, globMem m)
                |   CH_ENHMETAFILE mf       => (CF_ENHMETAFILE, toHENH mf)
                |   CH_OWNERDISPLAY m       => (CF_OWNERDISPLAY, globMem m)
                |   CH_DSPTEXT m            => (CF_DSPTEXT, globMem m)
                |   CH_DSPBITMAP m          => (CF_DSPBITMAP, globMem m)
                |   CH_DSPMETAFILEPICT m    => (CF_DSPMETAFILEPICT, globMem m)
                |   CH_DSPENHMETAFILE m     => (CF_DSPENHMETAFILE, globMem m)
                |   CH_PRIVATE(i, m)        => (CF_PRIVATE i, globMem m)
                |   CH_GDIOBJ(i, m)         => (CF_GDIOBJ i, globMem m)
                |   CH_REGISTERED(i, m)     => (CF_REGISTERED i, globMem m)
                |   CH_HDROP d              => (CF_HDROP, toHDRP d)
                |   CH_LOCALE m             => (CF_LOCALE, globMem m)
            val res = call2(user "SetClipboardData") (CLIPFORMAT, POINTER) POINTER (cf, data)
        in
            if fromCint res = 0
            then raiseSysErr ()
            else ()
        end

        fun GetClipboardData(f: ClipboardFormat): CLIPHANDLE =
        let
            (* The result of GetClipboardData is a handle, usually but not always an
               HGLOBAL pointing to a piece of memory. *)
            val res = call1 (user "GetClipboardData") (CLIPFORMAT) POINTER (f)
            val _ = checkResult (fromCint res <> 0)
            fun getMem(p: vol) =
            let
                val hg = fromHG p
                val len = GlobalSize hg
                val res = toWord8vec(GlobalLock hg, len)
            in
                GlobalUnlock hg;
                res
            end
            and getText(p: vol): string =
            let
                val hg = fromHG p
                val s = fromCstring(GlobalLock hg)
            in
                GlobalUnlock hg;
                s
            end
        in
            case f of
                CF_NONE             => CH_NONE
            |   CF_TEXT             => CH_TEXT(getText res)
            |   CF_BITMAP           => CH_BITMAP(fromHBIT res)
            |   CF_METAFILEPICT     =>
                let
                    val hg = fromHG res
                    val res = CH_METAFILEPICT(fromMFP(deref(GlobalLock hg)))
                in
                    GlobalUnlock hg;
                    res
                end
            |   CF_SYLK             => CH_SYLK(getMem res)
            |   CF_DIF              => CH_DIF(getMem res)
            |   CF_TIFF             => CH_TIFF(getMem res)
            |   CF_OEMTEXT          => CH_OEMTEXT(getText res)
            |   CF_DIB              => CH_DIB(getMem res)
            |   CF_PALETTE          => CH_PALETTE(fromHPAL res)
            |   CF_PENDATA          => CH_PENDATA(getMem res)
            |   CF_RIFF             => CH_RIFF(getMem res)
            |   CF_WAVE             => CH_WAVE(getMem res)
            |   CF_UNICODETEXT      => CH_UNICODETEXT(getMem res)
            |   CF_ENHMETAFILE      => CH_ENHMETAFILE(fromHENH res)
            |   CF_OWNERDISPLAY     => CH_OWNERDISPLAY(getMem res)
            |   CF_DSPTEXT          => CH_DSPTEXT(getMem res)
            |   CF_DSPBITMAP        => CH_DSPBITMAP(getMem res)
            |   CF_DSPMETAFILEPICT  => CH_DSPMETAFILEPICT(getMem res)
            |   CF_DSPENHMETAFILE   => CH_DSPENHMETAFILE(getMem res)
            |   CF_PRIVATE i        => CH_PRIVATE(i, getMem res)
            |   CF_GDIOBJ i         => CH_GDIOBJ(i, getMem res)
            |   CF_REGISTERED i     => CH_REGISTERED(i, getMem res)
            |   CF_HDROP            => CH_HDROP(fromHDRP res)
            |   CF_LOCALE           => CH_LOCALE(getMem res)
        end

        local
            val getformat = call3 (user "GetClipboardFormatNameA") (CLIPFORMAT, POINTER, INT) INT
        in
            (* Loop until we have read the whole string.  The result may legitimately be
               a null string. *)
            fun GetClipboardFormatName(f: ClipboardFormat): string =
                getStringCall(fn (buff, n) => getformat(f, buff, n))
        end

        fun GetPriorityClipboardFormat(l: ClipboardFormat list): ClipboardFormat option =
        let
            val (vec, count) = list2Vector CLIPFORMAT l
            val res = call2(user "GetPriorityClipboardFormat") (POINTER, INT) INT (vec, count)
            val (toClip, _, _) = breakConversion CLIPFORMAT
        in
            (* It returns 0 if the clipboard is empty, ~1 if it doesn't contain any
               of the requested formats and >0 if it contains one of the formats.
               We map ~1 to NONE. *)
            if res < 0 then NONE else SOME(toClip(toCint res))
        end
    end
end;
(*
Other clipboard functions:    
    GetClipboardSequenceNumber  - Windows 98 and NT 5.0 only
*)
