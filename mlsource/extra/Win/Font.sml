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
          DEFAULT_QUALITY
        | DRAFT_QUALITY
        | OTHER_QUALITY of int
        | PROOF_QUALITY
    and CharacterSet =
          ANSI_CHARSET
        | CHINESEBIG5_CHARSET
        | DEFAULT_CHARSET
        | HANGEUL_CHARSET
        | OEM_CHARSET
        | OTHER_CHARSET of int
        | SHIFTJIS_CHARSET
        | SYMBOL_CHARSET

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

    type ClippingPrecision
    val CLIP_CHARACTER_PRECIS : ClippingPrecision
    val CLIP_DEFAULT_PRECIS : ClippingPrecision
    val CLIP_EMBEDDED : ClippingPrecision
    val CLIP_LH_ANGLES : ClippingPrecision
    val CLIP_MASK : ClippingPrecision
    val CLIP_STROKE_PRECIS : ClippingPrecision
    val CLIP_TT_ALWAYS : ClippingPrecision

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
        clipPrecision : ClippingPrecision,
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
    val GetCharABCWidths : HDC * char * char -> (int * int * int) list
    val GetCharABCWidthsFloat : HDC * char * char -> (real * real * real) list
    val GetCharWidth32 : HDC * char * char -> int list
    val GetCharWidthFloat : HDC * int * int -> real list
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
        open CInterface Base GdiBase

        fun callgdi name = call_sym (load_sym (load_lib "gdi32.DLL") name)
        fun gdicall_IW name CR (C1,C2) (a1) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = address (alloc 1 ctype2)
                val res = callgdi name [(ctype1,va1),(Cpointer ctype2,va2)] ctypeR
                val _: unit = fromR res
            in  (from2 (deref va2))
            end
        val HEIGHT = INT: int Conversion
        val FONTMAPPERFLAG = BOOL : bool Conversion
    in
        type COLORREF = Color.COLORREF
        type SIZE = SIZE and POINT = POINT and RECT = RECT
        type HDC = HDC and HFONT = HFONT

        open FontBase (* Contains the types used in LOGFONT. *)

        datatype TextAlign = 
            TA_NOUPDATECP | TA_UPDATECP | TA_LEFT | TA_RIGHT | TA_CENTER | TA_TOP | TA_BOTTOM | TA_BASELINE
        local
            val tab = [
                (TA_NOUPDATECP,     0),
                (TA_UPDATECP,       1),
                (TA_LEFT,           0),
                (TA_RIGHT,          2),
                (TA_CENTER,         6),
                (TA_TOP,            0),
                (TA_BOTTOM,         8),
                (TA_BASELINE,       24)
            ]
        in
            val TEXTALIGN = tableSetConversion(tab, NONE)
        end
        
        (*TYPE: DrawTextMode *)
        datatype DrawTextMode = DT_TOP | DT_LEFT | DT_CENTER | DT_RIGHT | DT_VCENTER | DT_BOTTOM |
            DT_WORDBREAK | DT_SINGLELINE | DT_EXPANDTABS | DT_NOCLIP | DT_EXTERNALLEADING |
            DT_CALCRECT  | DT_NOPREFIX | DT_INTERNAL | DT_TABSTOP of int
        local
            val tab = [
                (DT_TOP,                0x0000),
                (DT_LEFT,               0x0000),
                (DT_CENTER,             0x0001),
                (DT_RIGHT,              0x0002),
                (DT_VCENTER,            0x0004),
                (DT_BOTTOM,             0x0008),
                (DT_WORDBREAK,          0x0010),
                (DT_SINGLELINE,         0x0020),
                (DT_EXPANDTABS,         0x0040),
                (DT_NOCLIP,             0x0100),
                (DT_EXTERNALLEADING,    0x0200),
                (DT_CALCRECT,           0x0400),
                (DT_NOPREFIX,           0x0800),
                (DT_INTERNAL,           0x1000)
            ]
            val tabStop = 0x0080
            fun toInt (DT_TABSTOP i) = IntInf.orb(tabStop, i*256) | toInt _ = raise Match
            fun fromInt i =
                if IntInf.andb(i, tabStop) = tabStop
                then DT_TABSTOP((i div 256) mod 256)
                else raise Match;
        in
            val DRAWTEXTMODE = tableSetConversion(tab, SOME(fromInt, toInt))
        end

        
        val AddFontResource            = call1(gdi "AddFontResourceA") (STRING) INT
        val CreateScalableFontResource =
            call4(gdi "CreateScalableFontResourceA") (INT,STRING,STRING,STRING) (SUCCESSSTATE "CreateScalableFontResource")
                                         
        val GetTextAlign               = call1(gdi "GetTextAlign") (HDC) TEXTALIGN
        val GetTextCharacterExtra      = call1(gdi "GetTextCharacterExtra") (HDC) INT
        val RemoveFontResource         = call1(gdi "RemoveFontResourceA") (STRING) (SUCCESSSTATE "RemoveFontResource")
        val SetMapperFlags             = call2(gdi "SetMapperFlags") (HDC,FONTMAPPERFLAG) FONTMAPPERFLAG
        val SetTextAlign               = call2(gdi "SetTextAlign") (HDC,TEXTALIGN) TEXTALIGN
        val SetTextCharacterExtra      = call2(gdi "SetTextCharacterExtra") (HDC,INT) INT
        val SetTextJustification       = call3(gdi "SetTextJustification") (HDC,INT,INT) (SUCCESSSTATE "SetTextJustification")
        val DrawText                   =
            call4(user "DrawTextA") (HDC,STRING,POINTERTO RECT,DRAWTEXTMODE) INT
        val GetTextColor = call1 (gdi "GetTextColor") (HDC) COLORREF
        and SetTextColor = call2 (gdi "SetTextColor") (HDC, COLORREF) COLORREF
        val GetAspectRatioFilterEx     = gdicall_IW "GetAspectRatioFilterEx" (SUCCESSSTATE "GetAspectRatioFilterEx") (HDC,SIZE)

        fun CreateFont({height: int, width: int, escapement: int, orientation: int,
                       weight: FontWeight, italic: bool, underline: bool, strikeOut: bool,
                       charSet: CharacterSet, outputPrecision: OutputPrecision,
                       clipPrecision: ClippingPrecision, quality: OutputQuality,
                       pitch: FontPitch, family: FontFamily, faceName: string}: LOGFONT) =
            call14 (gdi "CreateFontA") (INT, INT, INT, INT, FONTWEIGHT, BOOL, BOOL, BOOL,
                    CHARACTERSET, OUTPUTPRECISION, CLIPPINGPRECISION, OUTPUTQUALITY,
                    FONTPITCHANDFAMILY, STRING) HFONT
                (height, width, escapement, orientation, weight, italic, underline,
                 strikeOut, charSet, outputPrecision, clipPrecision, quality,
                 (pitch, family),
                 if size faceName > 31 then String.substring(faceName, 0, 31) else faceName)

        (* CreateFont and CreateFontIndirect take the same arguments in ML. *)
        val CreateFontIndirect =
            call1 (gdi "CreateFontIndirectA") (POINTERTO LOGFONT) HFONT

        datatype ExtendedTextMode = ETO_OPAQUE | ETO_CLIPPED | ETO_GLYPH_INDEX |
                    ETO_RTLREADING | ETO_IGNORELANGUAGE
        local
            val tab = [
                (ETO_OPAQUE,                   0x0002),
                (ETO_CLIPPED,                  0x0004),
                (ETO_GLYPH_INDEX,              0x0010),
                (ETO_RTLREADING,               0x0080),
                (ETO_IGNORELANGUAGE,           0x1000)
            ]
        in
            val EXTENDEDTEXTOUT = tableSetConversion(tab, NONE)
        end

        fun ExtTextOut (h,({x,y}:POINT),option,rect,text,gapl) =
        let         
            val slen = String.size text
            val (gaps, _) =
                case gapl of
                    [] => (toCint 0 (* Null *), 0)
                |   _ => list2Vector INT gapl
            (* The Rect is optional but really depends on the ETO_OPAQUE or ETO_CLIPPED
               options. *)
            val r =
                case rect of
                    NONE => toCint 0 (* Null *)
                |   SOME rect =>
                    let
                        val (_, to, _) = breakConversion (POINTERTO RECT)
                    in
                        to rect
                    end
        in call8 (gdi "ExtTextOutA")
                 (HDC,INT,INT,EXTENDEDTEXTOUT,POINTER,STRING,INT,POINTER)
                    (SUCCESSSTATE "ExtTextOut")
                 (h,x,y,option,r,text,slen, gaps)
        end
        
        fun GetCharABCWidths (h, c1: char, c2: char) = 
        let val count = ord c2 - ord c1
            val ABC = STRUCT3(INT, INT, INT)
            val (toABC, fromABC, abcStruct) = breakConversion ABC
            val abcarr = alloc count abcStruct
        
            val _: unit = call4 (gdi "GetCharABCWidthsA")
                            (HDC,INT,INT,POINTER) (SUCCESSSTATE "GetCharABCWidths")
                            (h, ord c1, ord c2,address abcarr)
            fun getElement i = toABC(offset i abcStruct abcarr)
        in
            List.tabulate(count, getElement)
        end

        fun GetCharABCWidthsFloat (h,c1,c2) = 
        let val count = ord c2 - ord c1
            val ABC = STRUCT3(FLOAT, FLOAT, FLOAT)
            val (toABC, fromABC, abcStruct) = breakConversion ABC
            val abcarr = alloc count abcStruct
        
            val res = call4 (gdi "GetCharABCWidthsFloatA")
                            (HDC,INT,INT,POINTER) (SUCCESSSTATE "GetCharABCWidthsFloat")
                            (h, ord c1, ord c2, address abcarr)
        
            fun getElement i = toABC(offset i abcStruct abcarr)
        
        in
            List.tabulate(count, getElement)
        end
        
        fun GetCharWidth32 (h,c1,c2) = 
        let val count = ord c2 - ord c1
            val arr = alloc count Cint
        
            val res = call4 (gdi "GetCharWidth32A")
                            (HDC,INT,INT,POINTER) (SUCCESSSTATE "GetCharWidth32")
                            (h, ord c1, ord c2,address arr)
            fun getElement i = fromCint(offset i Cint arr)
        in
            List.tabulate(count, getElement)
        end
        
        fun GetCharWidthFloat (h,c1,c2) = 
        let val count = c2-c1
            val arr = alloc count Cfloat
        
            val res = call4 (gdi "GetCharWidthFloatA")
                            (HDC,INT,INT,POINTER) (SUCCESSSTATE "GetCharWidthFloat")
                            (h,c1,c2,address arr)
        
            fun getElement i = fromCfloat(offset i Cfloat arr)
        in
            List.tabulate(count, getElement)
        end
        
        fun GetTextExtentPoint32 (h,s) = 
        let
            val (fromSize, _, sizeStruct) = breakConversion SIZE
            val sz = alloc 1 sizeStruct
            val slen = String.size s
            val _ = call4 (gdi "GetTextExtentPoint32A")
                            (HDC,STRING,INT,POINTER) (SUCCESSSTATE "GetTextExtentPoint32")
                            (h,s,slen,address sz)
        in
           fromSize sz
        end
        
        fun TextOut (h,({x,y}:POINT),s) = 
        let val len = String.size s
        in
           call5 (gdi "TextOutA")
                 (HDC,INT,INT,STRING,INT) (SUCCESSSTATE "TextOut")
                 (h,x,y,s,len)
        end


        datatype TextMetricPitch = TMPF_FIXED_PITCH | TMPF_VECTOR | TMPF_TRUETYPE | TMPF_DEVICE
        (* N.B. TMPF_FIXED_PITCH is included if the font is NOT fixed pitch!! *)
        type TEXTMETRIC =
            { height: int, ascent: int, descent: int, internalLeading: int, externalLeading: int,
              aveCharWidth: int, maxCharWidth: int, weight: int, overhang: int,
              digitizedAspectX: int, digitizedAspectY: int, firstChar: char, lastChar: char,
              defaultChar: char, breakChar: char, italic: bool, underlined: bool, struckOut: bool,
              pitch: TextMetricPitch list, family: FontFamily, charSet : CharacterSet }

        fun GetTextMetrics hdc : TEXTMETRIC =
        let
            val TEXTMETRIC = STRUCT20(LONG, LONG, LONG, LONG, LONG, LONG, LONG, LONG, LONG, LONG,
                                      LONG, CHAR, CHAR, CHAR, CHAR, CHAR, CHAR, CHAR, CHAR, CHAR)
            val (fromTm, toTm, tmStruct) = breakConversion TEXTMETRIC
            val buff = alloc 1 tmStruct
            val _:unit = call2 (gdi "GetTextMetricsA") (HDC, POINTER) (SUCCESSSTATE "GetTextMetrics")
                    (hdc, address buff)
            val (height, ascent, descent, internalLeading, externalLeading,
                  aveCharWidth, maxCharWidth, weight, overhang,
                  digitizedAspectX, digitizedAspectY, firstChar, lastChar,
                  defaultChar, breakChar, italic, underlined, struckOut,
                  pitchAndFamily, charSet) = fromTm buff
            val (fromChs, _, _) = breakConversion CHARACTERSET
            val family = toFamily(IntInf.andb(ord pitchAndFamily, 0xf0))
            val pitch =
                (if IntInf.andb(ord pitchAndFamily, 1) <> 0 then [TMPF_FIXED_PITCH] else []) @
                (if IntInf.andb(ord pitchAndFamily, 2) <> 0 then [TMPF_VECTOR] else []) @
                (if IntInf.andb(ord pitchAndFamily, 4) <> 0 then [TMPF_TRUETYPE] else []) @
                (if IntInf.andb(ord pitchAndFamily, 8) <> 0 then [TMPF_DEVICE] else []) 
        in
            {
            height = height, ascent = ascent, descent = ascent, internalLeading = internalLeading,
            externalLeading = externalLeading, aveCharWidth = aveCharWidth, maxCharWidth = maxCharWidth,
            weight = weight, overhang = overhang, digitizedAspectX = digitizedAspectX,
            digitizedAspectY = digitizedAspectY, firstChar = firstChar, lastChar = lastChar,
            defaultChar = defaultChar, breakChar = breakChar, italic = italic <> #"\000",
            underlined = underlined <> #"\000", struckOut = struckOut <> #"\000",
            family = family, pitch = pitch, charSet = fromChs(toCint(ord charSet))
            }
        end

        fun GetTextFace hdc : string =
        let
            val getFaceCall = call3(gdi "GetTextFaceA") (HDC, INT, POINTER) INT
            (* Call with a NULL buffer to find out the size. *)
            val count = getFaceCall(hdc, 0, toCint 0)
            val _ = checkResult(count >= 0)
            val buff = alloc count Cchar
            val res = getFaceCall(hdc, count, address buff)
        in
            fromCstring(address buff)
        end

        fun GetTextExtentExPoint(hdc: HDC, s: string, maxWidth: int option) :
            {fit: int option, extents: int list, size: SIZE} =
        let
            val count = size s
            val vec = alloc count Cint
            (* The lpnFit argument controls whether we get the maximum no. of chars. *)
            val lpnFit =
                case maxWidth of
                    NONE => toCint 0
                |   SOME f => address(alloc 1 Cint)
            val (fromSize, _, sizeStruct) = breakConversion SIZE
            val sizeVec = alloc 1 sizeStruct
            val _: unit = call7(gdi "GetTextExtentExPointA")
                (HDC, STRING, INT, INT, POINTER, POINTER, POINTER)
                    (SUCCESSSTATE "GetTextExtentExPoint")
                    (hdc, s, count, getOpt(maxWidth, 0), lpnFit, address vec, address sizeVec)
            val fit = case maxWidth of NONE => NONE | _ => SOME(fromCint(deref lpnFit))
            val extents = List.tabulate(getOpt(fit, count), fn i => fromCint(offset i Cint vec))
        in
            {fit = fit, extents = extents, size = fromSize sizeVec}
        end

        local
            val tabbedTextOut =
                call8 (user "TabbedTextOutA") (HDC, INT, INT, STRING, INT, INT, POINTER, INT) UINT
        in
            fun TabbedTextOut(hdc, {x, y}: POINT, str, tabs, origin): SIZE =
            let
                val (tabVec, nTabs) =
                    case tabs of
                        [] => (toCint 0, 0) (* Make the vector null. *)
                    | _ => list2Vector INT tabs
                val res = tabbedTextOut(hdc, x, y, str, size str, nTabs, tabVec, origin)
            in
                (* Zero represents an error.  But it's also possible to return zero if
                   the string is empty. *)
                {cx = LOWORD res, cy = HIWORD res}
            end
        end

        local
            val tabbedTextExtent =
                call5 (user "GetTabbedTextExtentA") (HDC, STRING, INT, INT, POINTER)
                    (POSINT "GetTabbedTextExtent")
        in
            fun GetTabbedTextExtent(hdc, str, tabs): SIZE =
            let
                val (tabVec, nTabs) =
                    case tabs of
                        [] => (toCint 0, 0) (* Make the vector null. *)
                    | _ => list2Vector INT tabs
                val res = tabbedTextExtent(hdc, str, size str, nTabs, tabVec)
            in
                {cx = LOWORD res, cy = HIWORD res}
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
