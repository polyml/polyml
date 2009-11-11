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

structure GdiBase =
struct
    local
        open CInterface Base
    in
        local
            datatype RasterOpCode =
            W of int
            and QuaternaryRop = Y of int
        in
            type RasterOpCode = RasterOpCode
            type QuaternaryRop = QuaternaryRop
            val RASTEROPCODE = absConversion {abs = W, rep = fn W n => n} INT
            val QUATERNARY = absConversion {abs = Y, rep = fn Y n => n} INT
        
            val SRCCOPY                                      = W (0x00CC0020 (* dest = source *))
            val SRCPAINT                                     = W (0x00EE0086 (* dest = source OR dest *))
            val SRCAND                                       = W (0x008800C6 (* dest = source AND dest *))
            val SRCINVERT                                    = W (0x00660046 (* dest = source XOR dest *))
            val SRCERASE                                     = W (0x00440328 (* dest = source AND (NOT dest ) *))
            val NOTSRCCOPY                                   = W (0x00330008 (* dest = (NOT source) *))
            val NOTSRCERASE                                  = W (0x001100A6 (* dest = (NOT src) AND (NOT dest) *))
            val MERGECOPY                                    = W (0x00C000CA (* dest = (source AND pattern) *))
            val MERGEPAINT                                   = W (0x00BB0226 (* dest = (NOT source) OR dest *))
            val PATCOPY                                      = W (0x00F00021 (* dest = pattern *))
            val PATPAINT                                     = W (0x00FB0A09 (* dest = DPSnoo *))
            val PATINVERT                                    = W (0x005A0049 (* dest = pattern XOR dest *))
            val DSTINVERT                                    = W (0x00550009 (* dest = (NOT dest) *))
            val BLACKNESS                                    = W (0x00000042 (* dest = BLACK *))
            val WHITENESS                                    = W (0x00FF0062 (* dest = WHITE *))

            fun MAKEROP4{fore = (W fore): RasterOpCode, back = (W back): RasterOpCode} =
                Y(IntInf.orb(fore, IntInf.andb(IntInf.<<(back, 0w8), 0xFF000000)))
        end


        (* BITMAPS *)
        type BITMAP =
            { width: int, height: int, widthBytes: int, planes: int, bitsPerPixel: int,
              bits: Word8Vector.vector option }
        local
            val bitmapStruct = STRUCT7(INT, INT, INT, INT, SHORT, SHORT, POINTER)
            val (fromCStr, toCStr, lpStruct) = breakConversion bitmapStruct

            fun toCbmp({width, height, widthBytes, planes, bitsPerPixel, bits}: BITMAP) =
                toCStr(0, width, height, widthBytes, planes, bitsPerPixel,
                    case bits of NONE => toCint 0 | SOME b => fromWord8vec b)

            fun fromCbmp(v: vol): BITMAP =
            let
                val (_, width, height, widthBytes, planes, bitsPerPixel, bits) =
                    fromCStr v
                val bits =
                    if fromCint bits = 0
                    then NONE
                    else SOME (toWord8vec (bits, height * widthBytes))
            in
                {width = width, height = height, widthBytes = widthBytes, planes = planes,
                 bitsPerPixel = bitsPerPixel, bits = bits}
            end
        in
            val BITMAP = mkConversion fromCbmp toCbmp lpStruct
        end

        (* Line and Path *)
        datatype PointType =
            PT_MOVETO | PT_LINETO | PT_BEZIERTO | PT_LINETOANDCLOSE | PT_BEZIERTOANDCLOSE
        local
            val tab = [
                (PT_LINETO,             2),
                (PT_BEZIERTO,           4),
                (PT_MOVETO,             6),
                (PT_LINETOANDCLOSE,     3),
                (PT_BEZIERTOANDCLOSE,   5)
            ]
            val (toInt, fromInt) = tableLookup(tab, NONE)
        in
            val POINTTYPE =
                absConversion {abs = fromInt o ord, rep = chr o toInt} CHAR (* Encoded as single bytes *)
        end

        (* COLORREF - this is an RGB encoded into a 32-bit word. *)
        abstype COLORREF = C of LargeWord.word
        with
            local
                open LargeWord
                infix 7 andb
                infix 6 orb
                infix 4 << >>
            in
                fun RGB{red: int, green: int, blue: int} =
                    C(fromInt red andb 0wxff
                      orb (fromInt green andb 0wxff << 0w8)
                      orb (fromInt blue andb 0wxff << 0w16))

                fun PALETTERGB rgb = let val C r = RGB rgb in C(r orb 0wx02000000) end
                    
                fun toRGB(C p) =
                    { red = toInt(p andb 0wxff),
                      green = toInt((p >> 0w8) andb 0wxff),
                      blue = toInt((p >> 0w16) andb 0wxff) }
            end
            val COLORREF = absConversion {abs=C, rep = fn(C v) => v} WORD
        end

        (* Brush *)

        datatype BrushStyle = BS_SOLID | BS_HOLLOW | BS_HATCHED of HatchStyle | BS_PATTERN of HBITMAP
            (* | BS_DIBPATTERN of PACKEDDIB *)
        and  HatchStyle =
            HS_HORIZONTAL | HS_VERTICAL | HS_FDIAGONAL | HS_BDIAGONAL | HS_CROSS | HS_DIAGCROSS

        type LOGBRUSH = BrushStyle * COLORREF
        local
            val LBRUSH = STRUCT3(INT, COLORREF, INT)
            val (fromStr, toStr, lbStruct) = breakConversion LBRUSH
            val hbtab = [
                (HS_HORIZONTAL,     0 (* ~~~~~ *)),
                (HS_VERTICAL,       1 (* ||||| *)),
                (HS_FDIAGONAL,      2 (* \\\\\ *)),
                (HS_BDIAGONAL,      3 (* (* /// *) *)),
                (HS_CROSS,          4 (* +++++ *)),
                (HS_DIAGCROSS,      5 (* xxxxx *))
            ]
            val (fromHB, toHB) = tableLookup(hbtab, NONE)

            fun toLB(BS_SOLID, cr) = toStr(0, cr, 0)
             |  toLB(BS_HOLLOW, cr) = toStr(1, cr (* actually ignored *), 0)
             |  toLB(BS_HATCHED hs, cr) = toStr(2, cr, fromHB hs)
             |  toLB(BS_PATTERN hb, cr) = toStr(3, cr (* actually ignored *), hgdiAsInt hb)
             (* |  toLB(BS_DIBPATTERN dp, cr) = toStr(5, cr (* treated specially *), ??? dp) *)

            fun fromLB (v: vol): LOGBRUSH =
            let
                val (t, cr, i) = fromStr v
            in
                case t of
                    0 => (BS_SOLID, cr)
                |   1 => (BS_HOLLOW, cr)
                |   2 => (BS_HATCHED(toHB i), cr)
                |   3 => (BS_PATTERN(intAsHgdi i), cr)
                |   _ => raise Fail "Unknown brush type"
            end
        in
            val HATCHSTYLE = absConversion {abs = toHB, rep = fromHB} INT
            val LOGBRUSH = mkConversion fromLB toLB lbStruct
        end

        (* Pen *)

        (* This is confused.  Many of these are only applicable for ExtCreatePen and most are
           mutually exclusive. *)
        datatype PenStyle = PS_SOLID | PS_DASH | PS_DOT | PS_DASHDOT | PS_DASHDOTDOT | PS_NULL |
            PS_INSIDEFRAME | PS_USERSTYLE | PS_ALTERNATE | PS_ENDCAP_ROUND | PS_ENDCAP_SQUARE |
            PS_ENDCAP_FLAT | PS_JOIN_ROUND | PS_JOIN_BEVEL | PS_JOIN_MITER | PS_COSMETIC | PS_GEOMETRIC


        type LOGPEN = PenStyle * int option * COLORREF

        local
            val LPEN = STRUCT3(INT, POINT, COLORREF)
            val (fromStr, toStr, lpStruct) = breakConversion LPEN
            val tab = [
                (PS_SOLID, 0),
                (PS_DASH, 1 (* ~~~~~~~ *)),
                (PS_DOT, 2 (* ....... *)),
                (PS_DASHDOT, 3 (* _._._._ *)),
                (PS_DASHDOTDOT, 4 (* _.._.._ *)),
                (PS_NULL, 5),
                (PS_INSIDEFRAME, 6),
                (PS_USERSTYLE, 7),
                (PS_ALTERNATE, 8),
                (PS_ENDCAP_ROUND, 0x00000000),
                (PS_ENDCAP_SQUARE, 0x00000100),
                (PS_ENDCAP_FLAT, 0x00000200),
                (PS_JOIN_ROUND, 0x00000000),
                (PS_JOIN_BEVEL, 0x00001000),
                (PS_JOIN_MITER, 0x00002000),
                (PS_COSMETIC, 0x00000000),
                (PS_GEOMETRIC, 0x00010000)
            ]
            val (fromPS, toPS) = tableLookup(tab, NONE)
            fun toLP((ps, width, cr): LOGPEN) =
                toStr(fromPS ps, {x=getOpt(width, 0), y=0}, cr)
            fun fromLP(v: vol): LOGPEN =
            let
                val (ps, {x=width, ...}, cr) = fromStr v
            in
                (toPS ps, case width of 0 => NONE | i => SOME i, cr)
            end
        in
            val PENSTYLE = tableSetConversion(tab, NONE)
            val LOGPEN = mkConversion fromLP toLP lpStruct
        end

        (* Transform *)
        datatype MapMode = MM_TEXT | MM_LOMETRIC | MM_HIMETRIC | MM_LOENGLISH | MM_HIENGLISH |
            MM_TWIPS | MM_ISOTROPIC | MM_ANISOTROPIC
        val MM_MIN                                       = MM_TEXT
        val MM_MAX                                       = MM_ANISOTROPIC
        val MM_MAX_FIXEDSCALE                            = MM_TWIPS
        
        local
            val tab = [
                (MM_TEXT,                                      1),
                (MM_LOMETRIC,                                  2),
                (MM_HIMETRIC,                                  3),
                (MM_LOENGLISH,                                 4),
                (MM_HIENGLISH,                                 5),
                (MM_TWIPS,                                     6),
                (MM_ISOTROPIC,                                 7),
                (MM_ANISOTROPIC,                               8)
            ]
            (* SetMapMode and GetMapMode return 0 in the event of an error. *)
            fun toInt _ = raise Match
            fun fromInt i = (checkResult(i <> 0); raise Match);
        in
            val MAPMODE = tableConversion(tab, SOME(fromInt, toInt))
        end

        (* REGIONS *)
        local
            datatype RegionOperation =
            W of int
        in
            type RegionOperation = RegionOperation
            val REGIONOPERATION  = absConversion {abs = W, rep = fn W n => n} INT
        
            val RGN_ERROR                                    = W (0)
            val RGN_AND                                      = W (1)
            val RGN_OR                                       = W (2)
            val RGN_XOR                                      = W (3)
            val RGN_DIFF                                     = W (4)
            val RGN_COPY                                     = W (5)
        end

        local
            datatype ResultRegion =
            W of int
        in
            type ResultRegion = ResultRegion
            val RESULTREGION  = absConversion {abs = W, rep = fn W n => n} INT
        
            val ERROR                                        = W (0)
            val NULLREGION                                   = W (1)
            val SIMPLEREGION                                 = W (2)
            val COMPLEXREGION                                = W (3)
        end


        type METAFILEPICT = {mm: MapMode, size: SIZE, hMF: HMETAFILE}

        local
            val metaFilePict = STRUCT3(MAPMODE, SIZE, HMETAFILE)
            val (toMfp, fromMfp, mfpStruct) = breakConversion metaFilePict
            fun toCMfp({mm, size, hMF}: METAFILEPICT): vol = fromMfp(mm, size, hMF)
            fun fromCMfp v : METAFILEPICT =
            let val (mm, size, hMF) = toMfp v in {mm=mm, size=size, hMF=hMF} end
        in
            (* This is needed in the Clipboard structure. *)
            val METAFILEPICT = mkConversion fromCMfp toCMfp mfpStruct
        end


    end
end;
