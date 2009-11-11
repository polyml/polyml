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

structure FontBase =
struct
    local
        open CInterface Base
    in
        datatype OutputQuality =
            DEFAULT_QUALITY | DRAFT_QUALITY | PROOF_QUALITY | OTHER_QUALITY of int
        local
            val tab = [
                (DEFAULT_QUALITY, 0),
                (DRAFT_QUALITY, 1),
                (PROOF_QUALITY, 2)
                ]
        in
            fun doConv (OTHER_QUALITY i) = i | doConv _ = raise Match
            val OUTPUTQUALITY = tableConversion(tab, SOME(OTHER_QUALITY, doConv))
        end

        datatype CharacterSet = ANSI_CHARSET | DEFAULT_CHARSET | SYMBOL_CHARSET |
                SHIFTJIS_CHARSET | HANGEUL_CHARSET | CHINESEBIG5_CHARSET | OEM_CHARSET |
                OTHER_CHARSET of int
        local
            val tab = [
                (ANSI_CHARSET, 0),
                (DEFAULT_CHARSET, 1),
                (SYMBOL_CHARSET, 2),
                (SHIFTJIS_CHARSET, 128),
                (HANGEUL_CHARSET, 129),
                (CHINESEBIG5_CHARSET, 136),
                (OEM_CHARSET, 255)
                ]
            fun doConv (OTHER_CHARSET i) = i | doConv _ = raise Match
        in
            val CHARACTERSET = tableConversion(tab, SOME(OTHER_CHARSET, doConv))
        end
    
        (* In the underlying CreateFont call the pitch and family are ORed together. *)
        (*TYPE: FontFamily *)
        datatype FontFamily = FF_DONTCARE | FF_ROMAN | FF_SWISS | FF_MODERN |
            FF_SCRIPT| FF_DECORATIVE

        and FontPitch = DEFAULT_PITCH | FIXED_PITCH | VARIABLE_PITCH

        local
            val tab1 = [
                (DEFAULT_PITCH, 0),
                (FIXED_PITCH, 1),
                (VARIABLE_PITCH, 2)]
            and tab2 = [
                (FF_DONTCARE, 0 * 16 (* (0<<4) Don't care or don't know. *)),
                (FF_ROMAN, 1 * 16 (* (1<<4) Variable stroke width, serifed. *)),
                (FF_SWISS, 2 * 16 (* (2<<4) Variable stroke width, sans~serifed. *)),
                (FF_MODERN, 3 * 16 (* (3<<4) Constant stroke width, serifed or sans~serifed. *)),
                (FF_SCRIPT, 4 * 16 (* (4<<4) Cursive, etc. *)),
                (FF_DECORATIVE, 5 * 16 (* (5<<4) Old English, etc. *))]
            val (fromPitch, toPitch) = tableLookup(tab1, NONE)
            and (fromFamily, toFamily) = tableLookup(tab2, NONE)
            fun repPF (pitch, family) = IntInf.orb(fromPitch pitch, fromFamily family)
            fun absPF i = (toPitch(IntInf.andb(i, 3)), toFamily(IntInf.andb(i, ~16)))
        in
            val toFamily = toFamily (* This is used in TEXTMETRIC. *)
            val FONTPITCHANDFAMILY = absConversion {abs = absPF, rep = repPF} INT
        end

        (*TYPE: FontWeight - This type is really int, not an abstract type. *)
        type FontWeight =  int
        val FONTWEIGHT = INT
        
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
                (OUT_DEFAULT_PRECIS, 0),
                (OUT_STRING_PRECIS, 1),
                (OUT_CHARACTER_PRECIS, 2),
                (OUT_STROKE_PRECIS, 3),
                (OUT_TT_PRECIS, 4),
                (OUT_DEVICE_PRECIS, 5),
                (OUT_RASTER_PRECIS, 6),
                (OUT_TT_ONLY_PRECIS, 7),
                (OUT_OUTLINE_PRECIS, 8),
                (OUT_SCREEN_OUTLINE_PRECIS, 9)
                ]
        in
            val OUTPUTPRECISION = tableConversion(tab, NONE)
        end

        (* TODO: This is a bit set. *)
        local
            datatype ClippingPrecision =
            W of int
        in
            type ClippingPrecision = ClippingPrecision
            val CLIPPINGPRECISION = absConversion {abs = W, rep = fn W n => n} INT
        
            val CLIP_DEFAULT_PRECIS                          = W (0)
            val CLIP_CHARACTER_PRECIS                        = W (1)
            val CLIP_STROKE_PRECIS                           = W (2)
            val CLIP_MASK                                    = W (0xf)
            val CLIP_LH_ANGLES                               = W (1 * 16 (* 1<<4 *))
            val CLIP_TT_ALWAYS                               = W (2 * 16 (* 2<<4 *))
            val CLIP_EMBEDDED                                = W (8 * 16 (* 8<<4 *))
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
            clipPrecision : ClippingPrecision,
            quality : OutputQuality,
            pitch: FontPitch,
            family: FontFamily,
            faceName : string
        }

        local
            val LogFont = STRUCT14(INT, INT, INT, INT, FONTWEIGHT, CHAR, CHAR, CHAR, CHAR,
                                CHAR, CHAR, CHAR, CHAR, CHARARRAY 32)
            val (fromBase, toBase, struc) = breakConversion LogFont
            val (fromCS, toCS, _) = breakConversion CHARACTERSET
            val (fromOP, toOP, _) = breakConversion OUTPUTPRECISION
            val (fromCP, toCP, _) = breakConversion CLIPPINGPRECISION
            val (fromOQ, toOQ, _) = breakConversion OUTPUTQUALITY
            val (fromFPF, toFPF, _) = breakConversion FONTPITCHANDFAMILY
            fun chToB #"\000" = false | chToB _ = true
            fun bToch false = #"\000" | bToch true = #"\001"

            fun from v : LOGFONT =
            let
                val (height, width, escapement, orientation, weight, italic, underline,
                 strikeOut, charSet, outputPrecision, clipPrecision, quality,
                 pitchFamily, faceName) = fromBase v
                val (pitch, family) = (fromFPF o toCint o ord) pitchFamily
            in
                {height = height, width = width, escapement = escapement,
                 orientation = orientation, weight = weight, italic = chToB italic,
                 underline = chToB underline, strikeOut = chToB strikeOut,
                 charSet = (fromCS o toCint o ord) charSet,
                 outputPrecision = (fromOP o toCint o ord) outputPrecision,
                 clipPrecision = (fromCP o toCint o ord) clipPrecision,
                 quality = (fromOQ o toCint o ord) quality, pitch = pitch, family = family,
                 faceName = faceName}
            end

            fun to ({height, width, escapement, orientation, weight, italic, underline,
                     strikeOut, charSet, outputPrecision, clipPrecision, quality,
                     pitch, family, faceName}: LOGFONT) =
            let
                val pitchFamily = fromCchar(toFPF(pitch, family))
            in
                toBase(height, width, escapement, orientation, weight, bToch italic,
                    bToch underline, bToch strikeOut, (chr o fromCint o toCS) charSet,
                    (chr o fromCint o toOP) outputPrecision,
                    (chr o fromCint o toCP) clipPrecision,
                    (chr o fromCint o toOQ) quality, pitchFamily, faceName)
            end
        in
            val LOGFONT = mkConversion from to struc
        end
    end
end;
