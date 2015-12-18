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

structure FontBase =
struct
    local
        open Foreign Base
    in
        datatype OutputQuality =
            DEFAULT_QUALITY | DRAFT_QUALITY | PROOF_QUALITY | ANTIALIASED_QUALITY | CLEARTYPE_QUALITY | NONANTIALIASED_QUALITY
        local
            val tab = [
                (DEFAULT_QUALITY,           0w0: Word8.word),
                (DRAFT_QUALITY,             0w1),
                (PROOF_QUALITY,             0w2),
                (NONANTIALIASED_QUALITY,    0w3),
                (ANTIALIASED_QUALITY,       0w4),
                (CLEARTYPE_QUALITY,         0w5)
                ]
        in
            val (outQualToW8, outQualFromW8) = tableLookup(tab, NONE)
        end

        datatype CharacterSet = ANSI_CHARSET | DEFAULT_CHARSET | SYMBOL_CHARSET | MAC_CHARSET |
                SHIFTJIS_CHARSET | HANGEUL_CHARSET | JOHAB_CHARSET | GB2312_CHARSET |
                CHINESEBIG5_CHARSET | GREEK_CHARSET | TURKISH_CHARSET | VIETNAMESE_CHARSET |
                HEBREW_CHARSET | ARABIC_CHARSET | BALTIC_CHARSET | RUSSIAN_CHARSET |
                THAI_CHARSET | EASTEUROPE_CHARSET | OEM_CHARSET

        local
            val tab = [
                (ANSI_CHARSET,          0wx00: Word8.word),
                (DEFAULT_CHARSET,       0wx01),
                (SYMBOL_CHARSET,        0wx02),
                (MAC_CHARSET,           0wx4D),
                (SHIFTJIS_CHARSET,      0wx80),
                (HANGEUL_CHARSET,       0wx81),
                (JOHAB_CHARSET,         0wx82),
                (GB2312_CHARSET,        0wx86),
                (CHINESEBIG5_CHARSET,   0wx88),
                (GREEK_CHARSET,         0wxA1),
                (TURKISH_CHARSET,       0wxA2),
                (VIETNAMESE_CHARSET,    0wxA3),
                (HEBREW_CHARSET,        0wxB1),
                (ARABIC_CHARSET,        0wxB2),
                (BALTIC_CHARSET,        0wxBA),
                (RUSSIAN_CHARSET,       0wxCC),
                (THAI_CHARSET,          0wxDE),
                (EASTEUROPE_CHARSET,    0wxEE),
                (OEM_CHARSET,           0wxff)
                ]
        in
            val (charsetToW8, charsetFromW8) = tableLookup(tab, NONE)
        end
    
        (* In the underlying CreateFont call the pitch and family are ORed together. *)
        (*TYPE: FontFamily *)
        datatype FontFamily = FF_DONTCARE | FF_ROMAN | FF_SWISS | FF_MODERN |
            FF_SCRIPT| FF_DECORATIVE

        and FontPitch = DEFAULT_PITCH | FIXED_PITCH | VARIABLE_PITCH

        local
            open Word8
            val tab1 = [
                (DEFAULT_PITCH,     0w0),
                (FIXED_PITCH,       0w1),
                (VARIABLE_PITCH,    0w2)]
            and tab2 = [
                (FF_DONTCARE,       0wx00 (* (0<<4) Don't care or don't know. *)),
                (FF_ROMAN,          0wx10 (* (1<<4) Variable stroke width, serifed. *)),
                (FF_SWISS,          0wx20 (* (2<<4) Variable stroke width, sans~serifed. *)),
                (FF_MODERN,         0wx30 (* (3<<4) Constant stroke width, serifed or sans~serifed. *)),
                (FF_SCRIPT,         0wx40 (* (4<<4) Cursive, etc. *)),
                (FF_DECORATIVE,     0wx50 (* (5<<4) Old English, etc. *))]
            val (fromPitch, toPitch) = tableLookup(tab1, NONE)
            and (fromFamily, toFamily) = tableLookup(tab2, NONE)
        in
            val toFamily = toFamily (* This is used in GetTextMetrics. *)
            fun pitchAndFamilyToW8 (pitch, family) = orb(fromPitch pitch, fromFamily family)
            fun pitchAndFamilyFromW8 i = (toPitch(andb(i, 0w3)), toFamily(andb(i, 0wxf0)))
        end

        (*TYPE: FontWeight - This type is really int, not an abstract type. *)
        type FontWeight =  int
        (* Values between 0 and 1000 *)
        (*val FONTWEIGHT = cLong*) (* It's int for CreateFont but LONG for LONGFONT. *)
        
        val FW_DONTCARE                                  = 0
        val FW_THIN                                      = 100
        val FW_EXTRALIGHT                                = 200
        val FW_LIGHT                                     = 300
        val FW_NORMAL                                    = 400
        val FW_MEDIUM                                    = 500
        val FW_SEMIBOLD                                  = 600
        val FW_BOLD                                      = 700
        val FW_EXTRABOLD                                 = 800
        val FW_HEAVY                                     = 900
        val FW_ULTRALIGHT                                = FW_EXTRALIGHT
        val FW_REGULAR                                   = FW_NORMAL
        val FW_DEMIBOLD                                  = FW_SEMIBOLD
        val FW_ULTRABOLD                                 = FW_EXTRABOLD
        val FW_BLACK                                     = FW_HEAVY
    
        datatype OutputPrecision = OUT_DEFAULT_PRECIS | OUT_STRING_PRECIS |
            OUT_CHARACTER_PRECIS | OUT_STROKE_PRECIS | OUT_TT_PRECIS | OUT_DEVICE_PRECIS |
            OUT_RASTER_PRECIS | OUT_TT_ONLY_PRECIS | OUT_OUTLINE_PRECIS |
            OUT_SCREEN_OUTLINE_PRECIS

        local
            val tab = [
                (OUT_DEFAULT_PRECIS,        0w0: Word8.word),
                (OUT_STRING_PRECIS,         0w1),
                (OUT_CHARACTER_PRECIS,      0w2),
                (OUT_STROKE_PRECIS,         0w3),
                (OUT_TT_PRECIS,             0w4),
                (OUT_DEVICE_PRECIS,         0w5),
                (OUT_RASTER_PRECIS,         0w6),
                (OUT_TT_ONLY_PRECIS,        0w7),
                (OUT_OUTLINE_PRECIS,        0w8),
                (OUT_SCREEN_OUTLINE_PRECIS, 0w9)
                ]
        in
            val (outPrecToW8, outPrecFromW8) = tableLookup(tab, NONE)
        end

        (* TODO: This is a bit set. *)
        datatype ClippingPrecision =
            CLIP_DEFAULT_PRECIS | CLIP_STROKE_PRECIS | CLIP_LH_ANGLES | CLIP_DFA_DISABLE | CLIP_EMBEDDED
            (* CLIP_CHARACTER_PRECIS and CLIP_TT_ALWAYS "should not be used"
               [CLIP_DEFAULT_PRECIS] is the same as [] i.e. zero. *)
        local
            val tab = [
                (CLIP_DEFAULT_PRECIS,       0wx0),
                (CLIP_STROKE_PRECIS,        0wx2),
                (CLIP_LH_ANGLES,            0wx10),
                (CLIP_DFA_DISABLE,          0w40),
                (CLIP_EMBEDDED,             0w80)
                ]
        in
            val (clipPrecSetToW32, clipPrecSetFromW32) = tableSetLookup(tab, NONE)
        end

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

        local
            val cLogFont =
                cStruct14(cLong, cLong, cLong, cLong, cLong, cUint8w, cUint8w, cUint8w, cUint8w,
                                cUint8w, cUint8w, cUint8w, cUint8w, cCHARARRAY 32)
            fun chToB 0w0 = false | chToB _ = true
            fun bToch false = 0w0 | bToch true = 0w1

            fun toLF(height, width, escapement, orientation, weight, italic, underline,
                    strikeOut, charSet, outputPrecision, clipPrecision, quality,
                    pitchFamily, faceName) : LOGFONT =
            let
                val (pitch, family) = pitchAndFamilyFromW8 pitchFamily
            in
                {height = height, width = width, escapement = escapement,
                 orientation = orientation, weight = weight, italic = chToB italic,
                 underline = chToB underline, strikeOut = chToB strikeOut,
                 charSet = charsetFromW8 charSet,
                 outputPrecision = outPrecFromW8 outputPrecision,
                 clipPrecision = clipPrecSetFromW32(Word32.fromLargeWord(Word8.toLargeWord clipPrecision)),
                 quality = outQualFromW8 quality, pitch = pitch, family = family,
                 faceName = faceName}
            end

            fun fromLF ({height, width, escapement, orientation, weight, italic, underline,
                     strikeOut, charSet, outputPrecision, clipPrecision, quality,
                     pitch, family, faceName}: LOGFONT) =
            let
                val pitchFamily = pitchAndFamilyToW8(pitch, family)
            in
                (height, width, escapement, orientation, weight, bToch italic,
                    bToch underline, bToch strikeOut, charsetToW8 charSet,
                    outPrecToW8 outputPrecision,
                    Word8.fromLargeWord(Word32.toLargeWord (clipPrecSetToW32 clipPrecision)),
                    outQualToW8 quality, pitchFamily, faceName)
            end
        in
            val cLOGFONT = absConversion{abs=toLF, rep=fromLF} cLogFont
        end
    end
end;
