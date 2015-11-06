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

structure Font :
  sig
    type HDC and HFONT
    type COLORREF = Color.COLORREF
    type POINT = {x: int, y: int}
    and  SIZE = {cx: int, cy: int }
    and RECT = { top: int, left: int, bottom: int, right: int }

    datatype FontFamily =
          FF_DECORATIVE
        | FF_DONTCARE
        | FF_MODERN
        | FF_ROMAN
        | FF_SCRIPT
        | FF_SWISS
    and FontPitch = DEFAULT_PITCH | FIXED_PITCH | VARIABLE_PITCH
    and OutputPrecision =
          OUT_CHARACTER_PRECIS
        | OUT_DEFAULT_PRECIS
        | OUT_DEVICE_PRECIS
        | OUT_OUTLINE_PRECIS
        | OUT_RASTER_PRECIS
        | OUT_SCREEN_OUTLINE_PRECIS
        | OUT_STRING_PRECIS
        | OUT_STROKE_PRECIS
        | OUT_TT_ONLY_PRECIS
        | OUT_TT_PRECIS
    and OutputQuality =
            DEFAULT_QUALITY | DRAFT_QUALITY | PROOF_QUALITY | ANTIALIASED_QUALITY | CLEARTYPE_QUALITY | NONANTIALIASED_QUALITY
    and CharacterSet =
          ANSI_CHARSET | DEFAULT_CHARSET | SYMBOL_CHARSET | MAC_CHARSET |
                SHIFTJIS_CHARSET | HANGEUL_CHARSET | JOHAB_CHARSET | GB2312_CHARSET |
                CHINESEBIG5_CHARSET | GREEK_CHARSET | TURKISH_CHARSET | VIETNAMESE_CHARSET |
                HEBREW_CHARSET | ARABIC_CHARSET | BALTIC_CHARSET | RUSSIAN_CHARSET |
                THAI_CHARSET | EASTEUROPE_CHARSET | OEM_CHARSET

    type FontWeight =  int
    val FW_BLACK : FontWeight
    val FW_BOLD : FontWeight
    val FW_DEMIBOLD : FontWeight
    val FW_DONTCARE : FontWeight
    val FW_EXTRABOLD : FontWeight
    val FW_EXTRALIGHT : FontWeight
    val FW_HEAVY : FontWeight
    val FW_LIGHT : FontWeight
    val FW_MEDIUM : FontWeight
    val FW_NORMAL : FontWeight
    val FW_REGULAR : FontWeight
    val FW_SEMIBOLD : FontWeight
    val FW_THIN : FontWeight
    val FW_ULTRABOLD : FontWeight
    val FW_ULTRALIGHT : FontWeight

    datatype ClippingPrecision =
        CLIP_DEFAULT_PRECIS | CLIP_STROKE_PRECIS | CLIP_LH_ANGLES | CLIP_DFA_DISABLE | CLIP_EMBEDDED

    type LOGFONT =
    {
        height : int,
        width : int,
        escapement : int,
        orientation : int,
        weight : FontWeight,
        italic : bool,
        underline : bool,
        strikeOut : bool,
        charSet : CharacterSet,
        outputPrecision: OutputPrecision,
        clipPrecision : ClippingPrecision list,
        quality : OutputQuality,
        pitch: FontPitch,
        family: FontFamily,
        faceName : string
    }

    datatype DrawTextMode =
          DT_BOTTOM
        | DT_CALCRECT
        | DT_CENTER
        | DT_EXPANDTABS
        | DT_EXTERNALLEADING
        | DT_INTERNAL
        | DT_LEFT
        | DT_NOCLIP
        | DT_NOPREFIX
        | DT_RIGHT
        | DT_SINGLELINE
        | DT_TABSTOP of int
        | DT_TOP
        | DT_VCENTER
        | DT_WORDBREAK
    and TextAlign =
          TA_BASELINE
        | TA_BOTTOM
        | TA_CENTER
        | TA_LEFT
        | TA_NOUPDATECP
        | TA_RIGHT
        | TA_TOP
        | TA_UPDATECP
    and TextMetricPitch =
          TMPF_DEVICE
        | TMPF_FIXED_PITCH
        | TMPF_TRUETYPE
        | TMPF_VECTOR
    and ExtendedTextMode =
          ETO_CLIPPED
        | ETO_GLYPH_INDEX
        | ETO_IGNORELANGUAGE
        | ETO_OPAQUE
        | ETO_RTLREADING

    type TEXTMETRIC =
            { height: int, ascent: int, descent: int, internalLeading: int, externalLeading: int,
              aveCharWidth: int, maxCharWidth: int, weight: int, overhang: int,
              digitizedAspectX: int, digitizedAspectY: int, firstChar: char, lastChar: char,
              defaultChar: char, breakChar: char, italic: bool, underlined: bool, struckOut: bool,
              pitch: TextMetricPitch list, family: FontFamily, charSet : CharacterSet }

    val AddFontResource : string -> int
    val CreateFont : LOGFONT -> HFONT
    val CreateFontIndirect : LOGFONT -> HFONT
    val CreateScalableFontResource : int * string * string * string -> unit
    val DrawText : HDC * string * RECT * DrawTextMode list -> int
    val ExtTextOut : HDC * POINT * ExtendedTextMode list *
       RECT option * string * int list -> unit
    val GetAspectRatioFilterEx : HDC -> SIZE
    val GetCharABCWidths : HDC * char * char -> (int * int * int) vector
    val GetCharABCWidthsFloat : HDC * char * char -> (real * real * real) vector
    val GetCharWidth32 : HDC * char * char -> int vector
    val GetCharWidthFloat : HDC * char * char -> real vector
    val GetTabbedTextExtent : HDC * string * int list -> SIZE
    val GetTextAlign : HDC -> TextAlign list
    val GetTextCharacterExtra : HDC -> int
    val GetTextColor : HDC -> COLORREF
    val GetTextExtentExPoint : HDC * string * int option ->
       {fit: int option, size: SIZE, extents: int list}
    val GetTextExtentPoint32 : HDC * string -> SIZE
    val GetTextFace : HDC -> string
    val GetTextMetrics : HDC -> TEXTMETRIC
    val RemoveFontResource : string -> unit
    val SetMapperFlags : HDC * bool -> bool
    val SetTextAlign : HDC * TextAlign list -> TextAlign list
    val SetTextCharacterExtra : HDC * int -> int
    val SetTextColor : HDC * COLORREF -> COLORREF
    val SetTextJustification : HDC * int * int -> unit
    val TabbedTextOut: HDC * POINT * string * int list * int -> SIZE
    val TextOut : HDC * POINT * String.string -> unit
  end
 =
struct
    local
        open Foreign Base GdiBase
        (*val HEIGHT = INT: int conversion*)
    in
        type COLORREF = Color.COLORREF
        type SIZE = SIZE and POINT = POINT and RECT = RECT
        type HDC = HDC and HFONT = HFONT

        open FontBase (* Contains the types used in LOGFONT. *)

        datatype TextAlign = 
            TA_NOUPDATECP | TA_UPDATECP | TA_LEFT | TA_RIGHT | TA_CENTER | TA_TOP | TA_BOTTOM | TA_BASELINE
        local
            val tab = [
                (TA_NOUPDATECP,     0w0),
                (TA_UPDATECP,       0w1),
                (TA_LEFT,           0w0),
                (TA_RIGHT,          0w2),
                (TA_CENTER,         0w6),
                (TA_TOP,            0w0),
                (TA_BOTTOM,         0w8),
                (TA_BASELINE,       0w24)
            ]
            val TEXTALIGN = tableSetConversion(tab, NONE)
        in
            val GetTextAlign               = winCall1(gdi "GetTextAlign") (cHDC) TEXTALIGN
            val SetTextAlign               = winCall2(gdi "SetTextAlign") (cHDC,TEXTALIGN) TEXTALIGN
        end
        
        (*TYPE: DrawTextMode *)
        datatype DrawTextMode = DT_TOP | DT_LEFT | DT_CENTER | DT_RIGHT | DT_VCENTER | DT_BOTTOM |
            DT_WORDBREAK | DT_SINGLELINE | DT_EXPANDTABS | DT_NOCLIP | DT_EXTERNALLEADING |
            DT_CALCRECT  | DT_NOPREFIX | DT_INTERNAL | DT_TABSTOP of int
        local
            val tab = [
                (DT_TOP,                0wx0000),
                (DT_LEFT,               0wx0000),
                (DT_CENTER,             0wx0001),
                (DT_RIGHT,              0wx0002),
                (DT_VCENTER,            0wx0004),
                (DT_BOTTOM,             0wx0008),
                (DT_WORDBREAK,          0wx0010),
                (DT_SINGLELINE,         0wx0020),
                (DT_EXPANDTABS,         0wx0040),
                (DT_NOCLIP,             0wx0100),
                (DT_EXTERNALLEADING,    0wx0200),
                (DT_CALCRECT,           0wx0400),
                (DT_NOPREFIX,           0wx0800),
                (DT_INTERNAL,           0wx1000)
            ]
            val tabStop = 0wx0080
            fun toInt (DT_TABSTOP i) = Word32.orb(tabStop, Word32.fromInt i*0w256) | toInt _ = raise Match
            fun fromInt i =
                if Word32.andb(i, tabStop) = tabStop
                then DT_TABSTOP(Word32.toInt(Word32.andb((Word32.>>(i, 0w8)), 0wxff)))
                else raise Match;
            val DRAWTEXTMODE = tableSetConversion(tab, SOME(fromInt, toInt))
        in
            val DrawText                   =
                winCall4(user "DrawTextA") (cHDC,cString,cConstStar cRect,DRAWTEXTMODE) cInt
        end

        val AddFontResource            = winCall1(gdi "AddFontResourceA") (cString) cInt
        val CreateScalableFontResource =
            winCall4(gdi "CreateScalableFontResourceA") (cDWORD,cString,cString,cString) (successState "CreateScalableFontResource")
                                         

        val GetTextCharacterExtra      = winCall1(gdi "GetTextCharacterExtra") (cHDC) cInt
        val RemoveFontResource         = winCall1(gdi "RemoveFontResourceA") (cString) (successState "RemoveFontResource")

        local
            val cFONTMAPPERFLAG: bool conversion =
                absConversion{rep=fn true => 0w1 | false => 0w0, abs=fn n => n <> 0w0} cDWORDw
        in
            val SetMapperFlags             = winCall2(gdi "SetMapperFlags") (cHDC, cFONTMAPPERFLAG) cFONTMAPPERFLAG
        end

        val SetTextCharacterExtra      = winCall2(gdi "SetTextCharacterExtra") (cHDC,cInt) cInt
        val SetTextJustification       = winCall3(gdi "SetTextJustification") (cHDC,cInt,cInt) (successState "SetTextJustification")
        val GetTextColor = winCall1 (gdi "GetTextColor") (cHDC) cCOLORREF
        and SetTextColor = winCall2 (gdi "SetTextColor") (cHDC, cCOLORREF) cCOLORREF
        
        local
            val getAspectRatioFilterEx =
                winCall2(gdi "GetAspectRatioFilterEx") (cHDC, cStar cSize)(successState "GetAspectRatioFilterEx")
        in
            fun GetAspectRatioFilterEx hdc =
            let
                val s = ref{cx=0, cy= 0}
            in
                getAspectRatioFilterEx(hdc, s);
                !s
            end
        end

        local
            val createFont =
                winCall14 (gdi "CreateFontA") (cInt, cInt, cInt, cInt, cInt (* FONTWEIGHT *), cDWORDw, cDWORDw, cDWORDw,
                        cDWORDw (*CHARACTERSET *), cDWORDw (* OUTPUTPRECISION *), cDWORDw (* CLIPPINGPRECISION *),
                        cDWORDw (* OUTPUTQUALITY *), cDWORDw (* FONTPITCHANDFAMILY *), cString) cHFONT
            fun bToch false = 0w0 | bToch true = 0w1
            val w8ToW32 = Word32.fromLargeWord o Word8.toLargeWord
        in
            fun CreateFont({height: int, width: int, escapement: int, orientation: int,
                           weight: FontWeight, italic: bool, underline: bool, strikeOut: bool,
                           charSet: CharacterSet, outputPrecision: OutputPrecision,
                           clipPrecision: ClippingPrecision list, quality: OutputQuality,
                           pitch: FontPitch, family: FontFamily, faceName: string}: LOGFONT) =
                    createFont(height, width, escapement, orientation, weight, bToch italic, bToch underline,
                     bToch strikeOut, w8ToW32(charsetToW8 charSet), w8ToW32(outPrecToW8 outputPrecision),
                     clipPrecSetToW32 clipPrecision, w8ToW32(outQualToW8 quality),
                     w8ToW32(pitchAndFamilyToW8 (pitch, family)),
                     if size faceName > 31 then String.substring(faceName, 0, 31) else faceName)
        end

        (* CreateFont and CreateFontIndirect take the same arguments in ML. *)
        val CreateFontIndirect =
            winCall1 (gdi "CreateFontIndirectA") (cConstStar cLOGFONT) cHFONT

        datatype ExtendedTextMode = ETO_OPAQUE | ETO_CLIPPED | ETO_GLYPH_INDEX |
                    ETO_RTLREADING | ETO_IGNORELANGUAGE
        local
            val tab = [
                (ETO_OPAQUE,                   0wx0002),
                (ETO_CLIPPED,                  0wx0004),
                (ETO_GLYPH_INDEX,              0wx0010),
                (ETO_RTLREADING,               0wx0080),
                (ETO_IGNORELANGUAGE,           0wx1000)
            ]
        in
            val EXTENDEDTEXTOUT = tableSetConversion(tab, NONE)
        end

        local
            val extTextOut =
                winCall8 (gdi "ExtTextOutA")
                 (cHDC,cInt,cInt, EXTENDEDTEXTOUT, cOptionPtr (cConstStar cRect), cString, cUint, cPointer)
                    (successState "ExtTextOut")
        in
            fun ExtTextOut (h,({x,y}:POINT), option, rect, text, gapl) =
            let         
                val slen = String.size text
                val (gaps, _) =
                    case gapl of
                        [] => (Memory.null, 0)
                    |   _ => list2Vector cInt gapl
                (* The Rect is optional but really depends on the ETO_OPAQUE or ETO_CLIPPED
                   options. *)
            in
                extTextOut(h, x, y, option, rect, text, slen, gaps)
                    handle ex => (Memory.free gaps; raise ex);
                Memory.free gaps
            end
        end

        local
            val ABC = cStruct3(cInt, cUint, cInt)
            val getCharABCWidths =
                winCall4 (gdi "GetCharABCWidthsA")
                            (cHDC, cUint, cUint, cPointer) (successState "GetCharABCWidths")
            val getVec = getVectorResult ABC
        in
            fun GetCharABCWidths (h, c1: char, c2: char) = 
            let
                fun getCharABC(abcarr, count) =
                    (getCharABCWidths(h, ord c1, ord c2, abcarr); count)
            in
                getVec getCharABC (ord c2 - ord c1 + 1) 
            end
        end

        local
            val ABC = cStruct3(cFloat, cFloat, cFloat)
            val getCharABCWidthsFloat =
                winCall4 (gdi "GetCharABCWidthsFloatA")
                            (cHDC, cUint, cUint, cPointer) (successState "GetCharABCWidthsFloat")
            val getVec = getVectorResult ABC
        in
            fun GetCharABCWidthsFloat (h,c1,c2) = 
            let
                fun getCharABC(abcarr, count) =
                    (getCharABCWidthsFloat(h, ord c1, ord c2, abcarr); count)
            in
                getVec getCharABC (ord c2 - ord c1 + 1)
            end
        end

        local
            val getCharWidth32 =
                winCall4 (gdi "GetCharWidth32A")
                                (cHDC, cUint, cUint, cPointer) (successState "GetCharWidth32")
            val getVec = getVectorResult cInt
        in
            fun GetCharWidth32 (h,c1,c2) = 
            let
                fun getCharW(vec, count) =
                    (getCharWidth32(h, ord c1, ord c2, vec); count)
            in
                getVec getCharW (ord c2 - ord c1 + 1)
            end
        end

        local
            val getCharWidthFloat =
                winCall4 (gdi "GetCharWidthFloatA")
                            (cHDC,cUint, cUint, cPointer) (successState "GetCharWidthFloat")
            val getVec = getVectorResult cFloat
        in
            fun GetCharWidthFloat (h,c1,c2) = 
            let
                fun getCharW(vec, count) =
                    (getCharWidthFloat(h, ord c1, ord c2, vec); count)
            in
                getVec getCharW (ord c2 - ord c1 + 1)
            end
        end
 
        local
            val getTextExtentPoint32 =
                winCall4 (gdi "GetTextExtentPoint32A")
                            (cHDC, cString, cInt, cStar cSize) (successState "GetTextExtentPoint32")
        in
            fun GetTextExtentPoint32 (h, s) = 
            let
                val r = ref {cx=0, cy=0}
                val () = getTextExtentPoint32(h, s, size s, r)
            in
               !r
            end
        end

        local
            val textOut =
                winCall5 (gdi "TextOutA")
                 (cHDC,cInt,cInt,cString,cInt) (successState "TextOut")
        in
            fun TextOut (h,({x,y}:POINT),s) = textOut(h, x, y, s, size s)
        end


        datatype TextMetricPitch = TMPF_FIXED_PITCH | TMPF_VECTOR | TMPF_TRUETYPE | TMPF_DEVICE
        
        (* N.B. TMPF_FIXED_PITCH is included if the font is NOT fixed pitch!! *)
        type TEXTMETRIC =
            { height: int, ascent: int, descent: int, internalLeading: int, externalLeading: int,
              aveCharWidth: int, maxCharWidth: int, weight: int, overhang: int,
              digitizedAspectX: int, digitizedAspectY: int, firstChar: char, lastChar: char,
              defaultChar: char, breakChar: char, italic: bool, underlined: bool, struckOut: bool,
              pitch: TextMetricPitch list, family: FontFamily, charSet : CharacterSet }
        local
            val TEXTMETRIC =
                cStruct20(cLong, cLong, cLong, cLong, cLong, cLong, cLong, cLong, cLong, cLong,
                          cLong, cChar, cChar, cChar, cChar, cUint8w, cUint8w, cUint8w, cUint8w, cUint8w)
            val getTextMetrics =
                winCall2 (gdi "GetTextMetricsA") (cHDC, cStar TEXTMETRIC) (successState "GetTextMetrics")
            val tmpfTab = [
                (TMPF_FIXED_PITCH,  0wx1), (* N.B.  This is the opposite *)
                (TMPF_VECTOR,       0wx2),
                (TMPF_TRUETYPE,     0wx4),
                (TMPF_DEVICE,       0wx8)
                ]
            val (_, tmpfFromW32) = tableSetLookup(tmpfTab, NONE)
        in

            fun GetTextMetrics hdc : TEXTMETRIC =
            let
                val r = ref (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #" ", #" ", #" ", #" ", 0w0, 0w0, 0w0, 0w0, 0w0)
                val () = getTextMetrics (hdc, r)
                val (height, ascent, descent, internalLeading, externalLeading,
                      aveCharWidth, maxCharWidth, weight, overhang,
                      digitizedAspectX, digitizedAspectY, firstChar, lastChar,
                      defaultChar, breakChar, italic, underlined, struckOut,
                      pitchAndFamily, charSet) = !r
                (*val (fromChs, _, _) = breakConversion CHARACTERSET*)
                val family = toFamily(Word8.andb(pitchAndFamily, 0wxf0))
                val pitch = tmpfFromW32(Word32.fromLargeWord(Word8.toLargeWord(Word8.andb(pitchAndFamily, 0wxf))))
            in
                {
                height = height, ascent = ascent, descent = descent, internalLeading = internalLeading,
                externalLeading = externalLeading, aveCharWidth = aveCharWidth, maxCharWidth = maxCharWidth,
                weight = weight, overhang = overhang, digitizedAspectX = digitizedAspectX,
                digitizedAspectY = digitizedAspectY, firstChar = firstChar, lastChar = lastChar,
                defaultChar = defaultChar, breakChar = breakChar, italic = italic <> 0w0,
                underlined = underlined <> 0w0, struckOut = struckOut <> 0w0,
                family = family, pitch = pitch, charSet = charsetFromW8 charSet
                }
            end
        end

        local
            val getFaceCall = winCall3(gdi "GetTextFaceA") (cHDC, cInt, cPointer) cInt
        in
            fun GetTextFace hdc : string =
                getStringWithNullIsLength(fn(vec, len) => getFaceCall(hdc, len, vec))
        end

        local
            val getTextExtentExPoint =
                winCall7(gdi "GetTextExtentExPointA")
                    (cHDC, cString, cInt, cInt, cPointer, cPointer, cStar cSize) (successState "GetTextExtentExPoint")
            val {load=loadInt, ctype={size=sizeInt, ...}, ...} = breakConversion cInt
            val sizeIntSw = SysWord.fromLargeWord(Word.toLargeWord sizeInt)
        in
            fun GetTextExtentExPoint(hdc: HDC, s: string, maxWidth: int option) :
                {fit: int option, extents: int list, size: SIZE} =
            let
                val count = size s
                open Memory
                val vec = malloc(Word.fromInt count * sizeInt)
                (* The lpnFit argument controls whether we get the maximum no. of chars. *)
                val lpnFit =
                    case maxWidth of
                        NONE => null
                    |   SOME _ => malloc sizeInt
                val sizeVec = ref {cx=0, cy=0}
                val () =
                    getTextExtentExPoint
                        (hdc, s, count, getOpt(maxWidth, 0), lpnFit, vec, sizeVec)
                            handle ex => (free vec; free lpnFit; raise ex)
                val fit = case maxWidth of NONE => NONE | _ => SOME(loadInt lpnFit)
                fun loadExt i =
                    loadInt(sysWord2VoidStar(voidStar2Sysword vec + SysWord.fromInt i * sizeIntSw))
                val extents = List.tabulate(getOpt(fit, count), loadExt)
                val () = free vec
                val () = free lpnFit
            in
                {fit = fit, extents = extents, size = ! sizeVec}
            end
        end

        local
            val tabbedTextOut =
                winCall8 (user "TabbedTextOutA") (cHDC, cInt, cInt, cString, cInt, cInt, cPointer, cInt) cDWORDw
        in
            fun TabbedTextOut(hdc, {x, y}: POINT, str, tabs, origin): SIZE =
            let
                val (tabVec, nTabs) =
                    case tabs of
                        [] => (Memory.null, 0) (* Make the vector null. *)
                    | _ => list2Vector cInt tabs
                val res =
                    tabbedTextOut(hdc, x, y, str, size str, nTabs, tabVec, origin)
                        handle ex => (Memory.free tabVec; raise ex)
                val () = Memory.free tabVec
                val () = checkResult(res <> 0w0)
            in
                (* Zero represents an error.  But it's also possible to return zero if
                   the string is empty. *)
                {cx = Word.toInt(LOWORD res), cy = Word.toInt(HIWORD res)}
            end
        end

        local
            val tabbedTextExtent =
                winCall5 (user "GetTabbedTextExtentA") (cHDC, cString, cInt, cInt, cPointer) cDWORDw
                    (*(POSINT "GetTabbedTextExtent")*)
        in
            fun GetTabbedTextExtent(hdc, str, tabs): SIZE =
            let
                val (tabVec, nTabs) =
                    case tabs of
                        [] => (Memory.null, 0) (* Make the vector null. *)
                    | _ => list2Vector cInt tabs
                val res =
                    tabbedTextExtent(hdc, str, size str, nTabs, tabVec)
                        handle ex => (Memory.free tabVec; raise ex)
                val () = Memory.free tabVec
                val () = checkResult(res <> 0w0)
            in
                {cx = Word.toInt(LOWORD res), cy = Word.toInt(HIWORD res)}
            end
        end

        (*
        Other Font and Text functions:
            DrawTextEx
            EnumFontFamiliesEx  
            EnumFontFamExProc
            GetCharacterPlacement  
            GetFontData  
            GetFontLanguageInfo  
            GetFontUnicodeRanges  
            GetGlyphIndices  
            GetGlyphOutline  
            GetKerningPairs  
            GetOutlineTextMetrics  
            GetRasterizerCaps  
            GetTabbedTextExtent  
            PolyTextOut  
        *)
            end
end;
