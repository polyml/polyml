(*
    Copyright (c) 2001, 2015
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
        open Foreign
        open Base
        val GMEM_SHARE = 0x2000
        and GMEM_MOVEABLE = 0x0002
        val GMEM_OPTS = IntInf.orb(GMEM_SHARE, GMEM_MOVEABLE)

        val {load=fromMFP, store=toMFP, ctype={size=sizeMfp, ...}, ...} = breakConversion GdiBase.cMETAFILEPICT
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
       
        local
            val (toInt, fromInt) = clipLookup
        in
            val cCLIPFORMAT = absConversion {abs = fromInt, rep = toInt} cUint
        end

        val ChangeClipboardChain = winCall2 (user "ChangeClipboardChain") (cHWND, cHWND) cBool
        and CloseClipboard = winCall0 (user "CloseClipboard") () (successState "CloseClipboard")
        and CountClipboardFormats = winCall0 (user "CountClipboardFormats") () cInt
        and EmptyClipboard = winCall0 (user "EmptyClipboard") () (successState "EmptyClipboard")
        and EnumClipboardFormats = winCall1 (user "EnumClipboardFormats") (cCLIPFORMAT) cCLIPFORMAT
        and GetClipboardOwner = winCall0 (user "GetClipboardOwner") () cHWND
        and GetClipboardViewer = winCall0 (user "GetClipboardViewer") () cHWND
        and GetOpenClipboardWindow = winCall0 (user "GetOpenClipboardWindow") () cHWND
        and IsClipboardFormatAvailable =
            winCall1 (user "IsClipboardFormatAvailable") (cCLIPFORMAT) cBool
        and OpenClipboard = winCall1 (user "OpenClipboard") (cHWNDOPT) (successState "OpenClipboard")
        and RegisterClipboardFormat =
            CF_REGISTERED o winCall1 (user "RegisterClipboardFormat") (cString) cUint
        and SetClipboardViewer = winCall1 (user "SetClipboardViewer") (cHWND) cHWND

        local
            (* The argument and result are actually HANDLE but we haven't got quite the
               right form of subclassing to allow all the various handle types to be combined. *)
            val setClipboardData = winCall2(user "SetClipboardData") (cCLIPFORMAT, cHGLOBAL) cHGLOBAL

            (* Most clipboard data is passed in memory allocated using GlobalAlloc. *)
            fun globString (s: string) =
            let
                val hGlob = GlobalAlloc(GMEM_OPTS, size s + 1)
                val mem = GlobalLock hGlob
            in
                copyStringToMem(mem, 0, s);
                GlobalUnlock hGlob;
                hGlob
            end
            and globMem (w: Word8Vector.vector) =
            let
                val length = Word8Vector.length w
                val hGlob = GlobalAlloc(GMEM_OPTS, length)
                val buf = GlobalLock hGlob
            in
                Word8Vector.appi (fn (i, v) => Memory.set8(buf, Word.fromInt i, v)) w;
                GlobalUnlock hGlob;
                hGlob
            end
            fun toHglobal (h: 'a HANDLE): HGLOBAL = handleOfVoidStar(voidStarOfHandle h)
        in
            (* SetClipboardData copies the data to the clipboard.  It is possible to pass
               NULL as the handle and instead process the WM_RENDERFORMAT message.  We
               don't support that. *)
            fun SetClipboardData(clip: CLIPHANDLE): unit =
            let

                (* Convert the various data formats and get the format type to pass. *)
                val (cf, data) =
                    case clip of
                        CH_NONE                 => raise Fail "SetClipboardData: No data"
                    |   CH_TEXT t               => (CF_TEXT, globString t)
                    |   CH_BITMAP b             => (CF_BITMAP, toHglobal b)
                    |   CH_METAFILEPICT p =>
                        let
                            val hGlob = GlobalAlloc(GMEM_OPTS, Word.toInt sizeMfp)
                        in
                            ignore(toMFP(GlobalLock hGlob, p));
                            GlobalUnlock hGlob;
                            (CF_METAFILEPICT, hGlob)
                        end
                    |   CH_SYLK m               => (CF_SYLK, globMem m)
                    |   CH_DIF m                => (CF_DIF, globMem m)
                    |   CH_TIFF m               => (CF_TIFF, globMem m)
                    |   CH_OEMTEXT t            => (CF_OEMTEXT, globString t)
                    |   CH_DIB m                => (CF_DIB, globMem m)
                    |   CH_PALETTE p            => (CF_PALETTE, toHglobal p)
                    |   CH_PENDATA m            => (CF_PENDATA, globMem m)
                    |   CH_RIFF m               => (CF_RIFF, globMem m)
                    |   CH_WAVE m               => (CF_WAVE, globMem m)
                    |   CH_UNICODETEXT m        => (CF_UNICODETEXT, globMem m)
                    |   CH_ENHMETAFILE mf       => (CF_ENHMETAFILE, toHglobal mf)
                    |   CH_OWNERDISPLAY m       => (CF_OWNERDISPLAY, globMem m)
                    |   CH_DSPTEXT m            => (CF_DSPTEXT, globMem m)
                    |   CH_DSPBITMAP m          => (CF_DSPBITMAP, globMem m)
                    |   CH_DSPMETAFILEPICT m    => (CF_DSPMETAFILEPICT, globMem m)
                    |   CH_DSPENHMETAFILE m     => (CF_DSPENHMETAFILE, globMem m)
                    |   CH_PRIVATE(i, m)        => (CF_PRIVATE i, globMem m)
                    |   CH_GDIOBJ(i, m)         => (CF_GDIOBJ i, globMem m)
                    |   CH_REGISTERED(i, m)     => (CF_REGISTERED i, globMem m)
                    |   CH_HDROP d              => (CF_HDROP, toHglobal d)
                    |   CH_LOCALE m             => (CF_LOCALE, globMem m)
                val res = setClipboardData (cf, data)
            in
                if res = hNull
                then raiseSysErr ()
                else ()
            end
        end

        local
            val getClipboardData = winCall1 (user "GetClipboardData") (cCLIPFORMAT) cHGLOBAL
            fun getMem hg = fromCWord8vec(GlobalLock hg, GlobalSize hg) before ignore(GlobalUnlock hg)
            and getText hg = fromCstring(GlobalLock hg) before ignore(GlobalUnlock hg)
            fun fromHglobal (h: HGLOBAL): 'a HANDLE = handleOfVoidStar(voidStarOfHandle h)
        in
            fun GetClipboardData(f: ClipboardFormat): CLIPHANDLE =
            let
                (* The result of GetClipboardData is a handle, usually but not always an
                   HGLOBAL pointing to a piece of memory. *)
                val res = getClipboardData f
                val _ = checkResult (res <> hNull)

            in
                case f of
                    CF_NONE             => CH_NONE
                |   CF_TEXT             => CH_TEXT(getText res)
                |   CF_BITMAP           => CH_BITMAP(fromHglobal res)
                |   CF_METAFILEPICT     =>
                        CH_METAFILEPICT(fromMFP(GlobalLock res)) before ignore(GlobalUnlock res)
                |   CF_SYLK             => CH_SYLK(getMem res)
                |   CF_DIF              => CH_DIF(getMem res)
                |   CF_TIFF             => CH_TIFF(getMem res)
                |   CF_OEMTEXT          => CH_OEMTEXT(getText res)
                |   CF_DIB              => CH_DIB(getMem res)
                |   CF_PALETTE          => CH_PALETTE(fromHglobal res)
                |   CF_PENDATA          => CH_PENDATA(getMem res)
                |   CF_RIFF             => CH_RIFF(getMem res)
                |   CF_WAVE             => CH_WAVE(getMem res)
                |   CF_UNICODETEXT      => CH_UNICODETEXT(getMem res)
                |   CF_ENHMETAFILE      => CH_ENHMETAFILE(fromHglobal res)
                |   CF_OWNERDISPLAY     => CH_OWNERDISPLAY(getMem res)
                |   CF_DSPTEXT          => CH_DSPTEXT(getMem res)
                |   CF_DSPBITMAP        => CH_DSPBITMAP(getMem res)
                |   CF_DSPMETAFILEPICT  => CH_DSPMETAFILEPICT(getMem res)
                |   CF_DSPENHMETAFILE   => CH_DSPENHMETAFILE(getMem res)
                |   CF_PRIVATE i        => CH_PRIVATE(i, getMem res)
                |   CF_GDIOBJ i         => CH_GDIOBJ(i, getMem res)
                |   CF_REGISTERED i     => CH_REGISTERED(i, getMem res)
                |   CF_HDROP            => CH_HDROP(fromHglobal res)
                |   CF_LOCALE           => CH_LOCALE(getMem res)
            end
        end

        local
            val getformat = winCall3 (user "GetClipboardFormatNameA") (cCLIPFORMAT, cPointer, cInt) cInt
        in
            (* Loop until we have read the whole string.  The result may legitimately be
               a null string. *)
            fun GetClipboardFormatName(f: ClipboardFormat): string =
                getStringCall(fn (buff, n) => getformat(f, buff, n))
        end

        local
            val getPriorityClipboardFormat = winCall2(user "GetPriorityClipboardFormat") (cPointer, cInt) cInt
         in
            fun GetPriorityClipboardFormat(l: ClipboardFormat list): ClipboardFormat option =
            let
                val (vec, count) = list2Vector cCLIPFORMAT l
                val res = getPriorityClipboardFormat(vec, count) handle ex => (Memory.free vec; raise ex)
                val () = Memory.free vec
            in
                (* It returns 0 if the clipboard is empty, ~1 if it doesn't contain any
                   of the requested formats and >0 if it contains one of the formats.
                   We map ~1 to NONE. *)
                if res < 0 then NONE else SOME(#2 clipLookup res)
            end
        end
    end
end;
(*
Other clipboard functions:    
    GetClipboardSequenceNumber  - Windows 98 and NT 5.0 only
*)
