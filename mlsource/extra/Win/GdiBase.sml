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

structure GdiBase =
struct
    local
        open Foreign Base
    in
        local
            datatype RasterOpCode =
            W of int
            and QuaternaryRop = Y of int
        in
            type RasterOpCode = RasterOpCode
            type QuaternaryRop = QuaternaryRop
            val cRASTEROPCODE = absConversion {abs = W, rep = fn W n => n} cDWORD
            val cQUATERNARY = absConversion {abs = Y, rep = fn Y n => n} cDWORD
        
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
            val bitmapStruct = cStruct7(cLong, cLong, cLong, cLong, cWORD, cWORD, cPointer)
            val {load = fromCStr, store = toCStr, ctype = lpStruct} = breakConversion bitmapStruct
            open Memory

            fun storeBmp(v: voidStar, {width, height, widthBytes, planes, bitsPerPixel, bits}: BITMAP) =
            let
                val m = case bits of NONE => Memory.null | SOME b => toCWord8vec b
            in
                toCStr(v, (0, width, height, widthBytes, planes, bitsPerPixel, m));
                fn () => Memory.free m
            end

            fun loadbmp(v: voidStar): BITMAP =
            let
                val (_, width, height, widthBytes, planes, bitsPerPixel, bits) =
                    fromCStr v
                val bits =
                    if bits = Memory.null
                    then NONE
                    else SOME (fromCWord8vec (bits, height * widthBytes))
            in
                {width = width, height = height, widthBytes = widthBytes, planes = planes,
                 bitsPerPixel = bitsPerPixel, bits = bits}
            end
        in
            val cBITMAP = makeConversion{store=storeBmp, load=loadbmp, ctype = lpStruct}
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
            val cPOINTTYPE =
                absConversion {abs = fromInt, rep = toInt} cUint8 (* Encoded as single bytes *)
        end

        (* COLORREF - this is an RGB encoded into a 32-bit word. *)
        abstype COLORREF = C of Word32.word
        with
            local
                open Word32
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
            val cCOLORREF = absConversion {abs=C, rep = fn(C v) => v} cDWORDw
        end

        (* Brush *)

        datatype BrushStyle = BS_SOLID | BS_HOLLOW | BS_HATCHED of HatchStyle | BS_PATTERN of HBITMAP
            (* | BS_DIBPATTERN of PACKEDDIB *)
        and  HatchStyle =
            HS_HORIZONTAL | HS_VERTICAL | HS_FDIAGONAL | HS_BDIAGONAL | HS_CROSS | HS_DIAGCROSS

        type LOGBRUSH = BrushStyle * COLORREF
        local
            val cLBRUSH = cStruct3(cUint, cCOLORREF, cULONG_PTR)
            val {load=loadStr, store=storeStr, ctype=lbStruct} = breakConversion cLBRUSH
            val hbtab = [
                (HS_HORIZONTAL,     0 (* ~~~~~ *)),
                (HS_VERTICAL,       1 (* ||||| *)),
                (HS_FDIAGONAL,      2 (* \\\\\ *)),
                (HS_BDIAGONAL,      3 (* (* /// *) *)),
                (HS_CROSS,          4 (* +++++ *)),
                (HS_DIAGCROSS,      5 (* xxxxx *))
            ]
            val (fromHB, toHB) = tableLookup(hbtab, NONE)
            val hgdiAsInt = SysWord.toInt o Memory.voidStar2Sysword o voidStarOfHandle
            and intAsHgdi = handleOfVoidStar o Memory.sysWord2VoidStar o SysWord.fromInt

            fun storeLB(m, (BS_SOLID, cr)) = storeStr(m, (0, cr, 0))
             |  storeLB(m, (BS_HOLLOW, cr)) = storeStr(m, (1, cr (* actually ignored *), 0))
             |  storeLB(m, (BS_HATCHED hs, cr)) = storeStr(m, (2, cr, fromHB hs))
             |  storeLB(m, (BS_PATTERN hb, cr)) =
                    storeStr(m, (3, cr (* actually ignored *), hgdiAsInt hb))
             (* |  toLB(BS_DIBPATTERN dp, cr) = toStr(5, cr (* treated specially *), ??? dp) *)

            fun loadLB (v: Memory.voidStar): LOGBRUSH =
            let
                val (t, cr, i) = loadStr v
            in
                case t of
                    0 => (BS_SOLID, cr)
                |   1 => (BS_HOLLOW, cr)
                |   2 => (BS_HATCHED(toHB i), cr)
                |   3 => (BS_PATTERN(intAsHgdi i), cr)
                |   _ => raise Fail "Unknown brush type"
            end
        in
            val cHATCHSTYLE = absConversion {abs = toHB, rep = fromHB} cInt
            val cLOGBRUSH = makeConversion{load=loadLB, store=storeLB, ctype = lbStruct}
        end

        (* Pen *)

        (* This is confused.  Many of these are only applicable for ExtCreatePen and most are
           mutually exclusive. *)
        datatype PenStyle = PS_SOLID | PS_DASH | PS_DOT | PS_DASHDOT | PS_DASHDOTDOT | PS_NULL |
            PS_INSIDEFRAME | PS_USERSTYLE | PS_ALTERNATE | PS_ENDCAP_ROUND | PS_ENDCAP_SQUARE |
            PS_ENDCAP_FLAT | PS_JOIN_ROUND | PS_JOIN_BEVEL | PS_JOIN_MITER | PS_COSMETIC | PS_GEOMETRIC


        type LOGPEN = PenStyle * int option * COLORREF

        local
            val LPEN = cStruct3(cUintw, cPoint, cCOLORREF)
            val {load=loadStr, store=storeStr, ctype=lpStruct} = breakConversion LPEN
            val tab = [
                (PS_SOLID, 0w0),
                (PS_DASH, 0w1 (* ~~~~~~~ *)),
                (PS_DOT, 0w2 (* ....... *)),
                (PS_DASHDOT, 0w3 (* _._._._ *)),
                (PS_DASHDOTDOT, 0w4 (* _.._.._ *)),
                (PS_NULL, 0w5),
                (PS_INSIDEFRAME, 0w6),
                (PS_USERSTYLE, 0w7),
                (PS_ALTERNATE, 0w8),
                (PS_ENDCAP_ROUND, 0wx00000000),
                (PS_ENDCAP_SQUARE, 0wx00000100),
                (PS_ENDCAP_FLAT, 0wx00000200),
                (PS_JOIN_ROUND, 0wx00000000),
                (PS_JOIN_BEVEL, 0wx00001000),
                (PS_JOIN_MITER, 0wx00002000),
                (PS_COSMETIC, 0wx00000000),
                (PS_GEOMETRIC, 0wx00010000)
            ]
            val (fromPS, toPS) = tableLookup(tab, NONE)

            fun storeLP(m, (ps, width, cr): LOGPEN) =
                storeStr(m, (fromPS ps, {x=getOpt(width, 0), y=0}, cr))

            fun loadLP v: LOGPEN =
            let
                val (ps, {x=width, ...}, cr) = loadStr v
            in
                (toPS ps, case width of 0 => NONE | i => SOME i, cr)
            end
        in
            val cPENSTYLE = tableSetConversion(tab, NONE)
            val cLOGPEN = makeConversion{store=storeLP, load=loadLP, ctype=lpStruct}
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
            val cMAPMODE = tableConversion(tab, SOME(fromInt, toInt)) cInt (* int for Get/SetMapMode *)
        end

        (* REGIONS *)
        local
            datatype RegionOperation =
            W of int
        in
            type RegionOperation = RegionOperation
            val REGIONOPERATION  = absConversion {abs = W, rep = fn W n => n} cInt
        
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
            val RESULTREGION  = absConversion {abs = W, rep = fn W n => n} cInt
        
            val ERROR                                        = W (0)
            val NULLREGION                                   = W (1)
            val SIMPLEREGION                                 = W (2)
            val COMPLEXREGION                                = W (3)
        end


        type METAFILEPICT = {mm: MapMode, size: SIZE, hMF: HMETAFILE}

        local
            val metaFilePict = cStruct3(cMAPMODE, cSize, cHMETAFILE)
            val {store=storeMfp, load=loadMfp, ctype=mfpStruct} = breakConversion metaFilePict
            fun storeCMfp(m, ({mm, size, hMF}: METAFILEPICT)) = storeMfp(m, (mm, size, hMF))
            fun loadCMfp v : METAFILEPICT =
            let val (mm, size, hMF) = loadMfp v in {mm=mm, size=size, hMF=hMF} end
        in
            (* This is needed in the Clipboard structure. *)
            val cMETAFILEPICT = makeConversion{store=storeCMfp, load=loadCMfp, ctype=mfpStruct}
        end


    end
end;
