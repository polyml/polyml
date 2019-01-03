(*
    Copyright (c) 2001, 2015, 2019
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

structure DeviceBase =
struct
    local
        open Foreign Base
    in
        (* Paper sizes. *)
        (* Colours.  Retain the American spelling for compatibility. *)
        datatype DMColor = DMCOLOR_MONOCHROME | DMCOLOR_COLOR
        local
            val tab = [
                (DMCOLOR_MONOCHROME,        1),
                (DMCOLOR_COLOR,             2)]
        in
            val (fromDMC, toDMC) = tableLookup(tab, NONE)
        end

        (* Default source. *)
        datatype DMSource = DMBIN_UPPER | DMBIN_ONLYONE | DMBIN_LOWER | DMBIN_MIDDLE | DMBIN_MANUAL |
                            DMBIN_ENVELOPE | DMBIN_ENVMANUAL | DMBIN_AUTO | DMBIN_TRACTOR |
                            DMBIN_SMALLFMT | DMBIN_LARGEFMT | DMBIN_LARGECAPACITY | DMBIN_CASSETTE |
                            DMBIN_FORMSOURCE | DMSOURCE_OTHER of int
        local
            val tab = [
                (DMBIN_ONLYONE, 1),
                (DMBIN_UPPER, 1),
                (DMBIN_LOWER, 2),
                (DMBIN_MIDDLE, 3),
                (DMBIN_MANUAL, 4),
                (DMBIN_ENVELOPE, 5),
                (DMBIN_ENVMANUAL, 6),
                (DMBIN_AUTO, 7),
                (DMBIN_TRACTOR, 8),
                (DMBIN_SMALLFMT, 9),
                (DMBIN_LARGEFMT, 10),
                (DMBIN_LARGECAPACITY, 11),
                (DMBIN_CASSETTE, 14),
                (DMBIN_FORMSOURCE, 15)]
        in
            fun doConv (DMSOURCE_OTHER i) = i | doConv _ = raise Match
            val (fromDMS, toDMS) = tableLookup(tab, SOME(DMSOURCE_OTHER, doConv))
        end
        (* Print quality.  Positive numbers represent dots per inch. *)
        datatype DMResolution = DMRES_DRAFT | DMRES_LOW | DMRES_MEDIUM | DMRES_HIGH | DMRES_DPI of int
        local
            val tab = [
                    (DMRES_DRAFT, ~1),
                    (DMRES_LOW, ~2),
                    (DMRES_MEDIUM, ~3),
                    (DMRES_HIGH, ~4)]
        in
            fun doConv (DMRES_DPI i) = i | doConv _ = raise Match
            val (fromDMR, toDMR) = tableLookup(tab, SOME(DMRES_DPI, doConv))
        end

        datatype DMDuplex = DMDUP_SIMPLEX | DMDUP_VERTICAL | DMDUP_HORIZONTAL
        local
            val tab = [
                    (DMDUP_SIMPLEX, 1),
                    (DMDUP_VERTICAL, 2),
                    (DMDUP_HORIZONTAL, 3)]
        in
            val (fromDMD, toDMD) = tableLookup(tab, NONE)
        end

        datatype DMTrueType = DMTT_BITMAP | DMTT_DOWNLOAD | DMTT_SUBDEV | DMTT_DOWNLOAD_OUTLINE
        local
            val tab = [
                    (DMTT_BITMAP, 1),
                    (DMTT_DOWNLOAD, 2),
                    (DMTT_SUBDEV, 3),
                    (DMTT_DOWNLOAD_OUTLINE, 4)]
        in
            val (fromDMTT, toDMTT) = tableLookup(tab, NONE)
        end

        datatype DMICMMethod = DMICMMETHOD_NONE | DMICMMETHOD_SYSTEM | DMICMMETHOD_DRIVER |
                               DMICMMETHOD_DEVICE | DMICMMETHOD_OTHER of int
        local
            val tab = [
                    (DMICMMETHOD_NONE, 1),
                    (DMICMMETHOD_SYSTEM, 2),
                    (DMICMMETHOD_DRIVER, 3),
                    (DMICMMETHOD_DEVICE, 4)]
        in
            fun doConv (DMICMMETHOD_OTHER i) = i | doConv _ = raise Match
            val (fromDMICMM, toDMICMM) = tableLookup(tab, SOME(DMICMMETHOD_OTHER, doConv))
        end

        datatype DMICMIntent = DMICM_SATURATE | DMICM_CONTRAST | DMICM_COLORMETRIC |
                               DMICMINTENT_OTHER of int
        local
            val tab = [
                    (DMICM_SATURATE, 1),
                    (DMICM_CONTRAST, 2),
                    (DMICM_COLORMETRIC, 3)]
        in
            fun doConv (DMICMINTENT_OTHER i) = i | doConv _ = raise Match
            val (fromDMICMI, toDMICMI) = tableLookup(tab, SOME(DMICMINTENT_OTHER, doConv))
        end

        datatype DMMedia = DMMEDIA_STANDARD | DMMEDIA_TRANSPARENCY | DMMEDIA_GLOSSY | DMICMMEDIA_OTHER of int
        local
            val tab = [
                    (DMMEDIA_STANDARD, 1),
                    (DMMEDIA_TRANSPARENCY, 2),
                    (DMMEDIA_GLOSSY, 3)]
        in
            fun doConv (DMICMMEDIA_OTHER i) = i | doConv _ = raise Match
            val (fromDMM, toDMM) = tableLookup(tab, SOME(DMICMMEDIA_OTHER, doConv))
        end

        datatype DMDither = DMDITHER_NONE | DMDITHER_COARSE | DMDITHER_FINE | DMDITHER_LINEART |
                            DMDITHER_GRAYSCALE | DMDITHER_OTHER of int
        local
            val tab = [
                    (DMDITHER_NONE, 1),
                    (DMDITHER_COARSE, 2),
                    (DMDITHER_FINE, 3),
                    (DMDITHER_LINEART, 4),
                    (DMDITHER_GRAYSCALE, 5)]
        in
            fun doConv (DMDITHER_OTHER i) = i | doConv _ = raise Match
            val (fromDMDi, toDMDi) = tableLookup(tab, SOME(DMDITHER_OTHER, doConv))
        end

        (* Paper orientation. *)
        datatype DMOrientation = DMORIENT_PORTRAIT | DMORIENT_LANDSCAPE
        local
            val tab = [
                (DMORIENT_PORTRAIT,     1),
                (DMORIENT_LANDSCAPE,    2)]
        in
            (* Because we use getShort to get the values we don't need a Conversion. *)
            val (fromDMO, toDMO) = tableLookup(tab, NONE)
        end

        datatype DMPaperSize = DMPAPER_LETTER | DMPAPER_LETTERSMALL | DMPAPER_TABLOID | DMPAPER_LEDGER |
            DMPAPER_LEGAL | DMPAPER_STATEMENT | DMPAPER_EXECUTIVE | DMPAPER_A3 | DMPAPER_A4 |
            DMPAPER_A4SMALL | DMPAPER_A5 | DMPAPER_B4 | DMPAPER_B5 | DMPAPER_FOLIO | DMPAPER_QUARTO |
            DMPAPER_10X14 | DMPAPER_11X17 | DMPAPER_NOTE | DMPAPER_ENV_9 | DMPAPER_ENV_10 | DMPAPER_ENV_11 |
            DMPAPER_ENV_12 | DMPAPER_ENV_14 | DMPAPER_CSHEET | DMPAPER_DSHEET | DMPAPER_ESHEET |
            DMPAPER_ENV_DL | DMPAPER_ENV_C5 | DMPAPER_ENV_C3 | DMPAPER_ENV_C4 | DMPAPER_ENV_C6 |
            DMPAPER_ENV_C65 | DMPAPER_ENV_B4 | DMPAPER_ENV_B5 | DMPAPER_ENV_B6 | DMPAPER_ENV_ITALY |
            DMPAPER_ENV_MONARCH | DMPAPER_ENV_PERSONAL | DMPAPER_FANFOLD_US | DMPAPER_FANFOLD_STD_GERMAN |
            DMPAPER_FANFOLD_LGL_GERMAN | DMPAPER_ISO_B4 | DMPAPER_JAPANESE_POSTCARD | DMPAPER_9X11 |
            DMPAPER_10X11 | DMPAPER_15X11 | DMPAPER_ENV_INVITE | DMPAPER_RESERVED_48 | DMPAPER_RESERVED_49 |
            DMPAPER_LETTER_EXTRA | DMPAPER_LEGAL_EXTRA | DMPAPER_TABLOID_EXTRA | DMPAPER_A4_EXTRA |
            DMPAPER_LETTER_TRANSVERSE | DMPAPER_A4_TRANSVERSE | DMPAPER_LETTER_EXTRA_TRANSVERSE |
            DMPAPER_A_PLUS | DMPAPER_B_PLUS | DMPAPER_LETTER_PLUS | DMPAPER_A4_PLUS |
            DMPAPER_A5_TRANSVERSE | DMPAPER_B5_TRANSVERSE | DMPAPER_A3_EXTRA | DMPAPER_A5_EXTRA |
            DMPAPER_B5_EXTRA | DMPAPER_A2 | DMPAPER_A3_TRANSVERSE | DMPAPER_A3_EXTRA_TRANSVERSE |
            DMPAPER_OTHER of int 

        local
            val tab = [
                (DMPAPER_LETTER, 1),
                (DMPAPER_LETTERSMALL, 2),
                (DMPAPER_TABLOID, 3),
                (DMPAPER_LEDGER, 4),
                (DMPAPER_LEGAL, 5),
                (DMPAPER_STATEMENT, 6),
                (DMPAPER_EXECUTIVE, 7),
                (DMPAPER_A3, 8),
                (DMPAPER_A4, 9),
                (DMPAPER_A4SMALL, 10),
                (DMPAPER_A5, 11),
                (DMPAPER_B4, 12),
                (DMPAPER_B5, 13),
                (DMPAPER_FOLIO, 14),
                (DMPAPER_QUARTO, 15),
                (DMPAPER_10X14, 16),
                (DMPAPER_11X17, 17),
                (DMPAPER_NOTE, 18),
                (DMPAPER_ENV_9, 19),
                (DMPAPER_ENV_10, 20),
                (DMPAPER_ENV_11, 21),
                (DMPAPER_ENV_12, 22),
                (DMPAPER_ENV_14, 23),
                (DMPAPER_CSHEET, 24),
                (DMPAPER_DSHEET, 25),
                (DMPAPER_ESHEET, 26),
                (DMPAPER_ENV_DL, 27),
                (DMPAPER_ENV_C5, 28),
                (DMPAPER_ENV_C3, 29),
                (DMPAPER_ENV_C4, 30),
                (DMPAPER_ENV_C6, 31),
                (DMPAPER_ENV_C65, 32),
                (DMPAPER_ENV_B4, 33),
                (DMPAPER_ENV_B5, 34),
                (DMPAPER_ENV_B6, 35),
                (DMPAPER_ENV_ITALY, 36),
                (DMPAPER_ENV_MONARCH, 37),
                (DMPAPER_ENV_PERSONAL, 38),
                (DMPAPER_FANFOLD_US, 39),
                (DMPAPER_FANFOLD_STD_GERMAN, 40),
                (DMPAPER_FANFOLD_LGL_GERMAN, 41),
                (DMPAPER_ISO_B4, 42),
                (DMPAPER_JAPANESE_POSTCARD, 43),
                (DMPAPER_9X11, 44),
                (DMPAPER_10X11, 45),
                (DMPAPER_15X11, 46),
                (DMPAPER_ENV_INVITE, 47),
                (DMPAPER_RESERVED_48, 48),
                (DMPAPER_RESERVED_49, 49),
                (DMPAPER_LETTER_EXTRA, 50),
                (DMPAPER_LEGAL_EXTRA, 51),
                (DMPAPER_TABLOID_EXTRA, 52),
                (DMPAPER_A4_EXTRA, 53),
                (DMPAPER_LETTER_TRANSVERSE, 54),
                (DMPAPER_A4_TRANSVERSE, 55),
                (DMPAPER_LETTER_EXTRA_TRANSVERSE, 56),
                (DMPAPER_A_PLUS, 57),
                (DMPAPER_B_PLUS, 58),
                (DMPAPER_LETTER_PLUS, 59),
                (DMPAPER_A4_PLUS, 60),
                (DMPAPER_A5_TRANSVERSE, 61),
                (DMPAPER_B5_TRANSVERSE, 62),
                (DMPAPER_A3_EXTRA, 63),
                (DMPAPER_A5_EXTRA, 64),
                (DMPAPER_B5_EXTRA, 65),
                (DMPAPER_A2, 66),
                (DMPAPER_A3_TRANSVERSE, 67),
                (DMPAPER_A3_EXTRA_TRANSVERSE, 68) ]
        in
            (* Because we use getShort to get the values we don't need a Conversion. *)
            fun doConv (DMPAPER_OTHER i) = i | doConv _ = raise Match
            val (fromDMPS, toDMPS) = tableLookup(tab, SOME(DMPAPER_OTHER, doConv))
        end

        type DEVMODE = {
            deviceName: string,
            driverVersion: int,
            orientation: DMOrientation option,
            paperSize: DMPaperSize option,
            paperLength: int option,
            paperWidth: int option,
            scale: int option,
            copies: int option,
            defaultSource: DMSource option,
            printQuality: DMResolution option,
            color: DMColor option,
            duplex: DMDuplex option,
            yResolution: int option,
            ttOption: DMTrueType option,
            collate: bool option,
            formName: string option,
            logPixels: int option,
            bitsPerPixel: int option,
            pelsWidth: int option,
            pelsHeight: int option,
            displayFlags: int option, (* Apparently no longer used. *)
            displayFrequency: int option,
            icmMethod: DMICMMethod option,
            icmIntent: DMICMIntent option,
            mediaType: DMMedia option,
            ditherType: DMDither option,
            panningWidth: int option,
            panningHeight: int option,
            driverPrivate: Word8Vector.vector
            }

        local
            val DM_SPECVERSION = 0x0401
            (* The size of the structure is the same in both 32-bit and 64-bit modes
                but is larger in Unicode (220 bytes). *)
            val DMBaseSize = 0w156 (* Size of structure without any user data. *)

            (* These bits indicate the valid fields in the structure. *)
            val DM_ORIENTATION      = 0x00000001
            val DM_PAPERSIZE        = 0x00000002
            val DM_PAPERLENGTH      = 0x00000004
            val DM_PAPERWIDTH       = 0x00000008
            val DM_SCALE            = 0x00000010
            val DM_COPIES           = 0x00000100
            val DM_DEFAULTSOURCE    = 0x00000200
            val DM_PRINTQUALITY     = 0x00000400
            val DM_COLOR            = 0x00000800
            val DM_DUPLEX           = 0x00001000
            val DM_YRESOLUTION      = 0x00002000
            val DM_TTOPTION         = 0x00004000
            val DM_COLLATE          = 0x00008000
            val DM_FORMNAME         = 0x00010000
            val DM_LOGPIXELS        = 0x00020000
            val DM_BITSPERPEL       = 0x00040000
            val DM_PELSWIDTH        = 0x00080000
            val DM_PELSHEIGHT       = 0x00100000
            val DM_DISPLAYFLAGS     = 0x00200000
            val DM_DISPLAYFREQUENCY = 0x00400000
            val DM_PANNINGWIDTH     = 0x00800000
            val DM_PANNINGHEIGHT    = 0x01000000
            val DM_ICMMETHOD        = 0x02000000
            val DM_ICMINTENT        = 0x04000000
            val DM_MEDIATYPE        = 0x08000000
            val DM_DITHERTYPE       = 0x10000000

            open Memory
            infix 6 ++
            
            val {load=loadShort, store=storeShort, ctype={size=sizeShort, ...}} =
                breakConversion cShort
            val {load=loadDWord, store=storeDWord, ctype={size=sizeDWord, ...}} =
                breakConversion cDWORD

            (* We need separate versions of this for local and global storage. PageSetupDlg
               requires a HGLOBAL handle to the memory. *)
            fun getCDevMode(v: voidStar) : DEVMODE =
            let
                val ptr = ref v

                fun getShort() = loadShort(!ptr) before ptr := !ptr ++ sizeShort
                and getDWord() = loadDWord(!ptr) before ptr := !ptr ++ sizeDWord

                val deviceName = fromCstring (!ptr)
                val () = ptr := !ptr ++ 0w32
                val _                   = getShort()
                val driverVersion       = getShort()
                val _                   = getShort()
                val driverExtra         = getShort()
                (* The "fields" value determines which of the fields are valid. *)
                val fields              = getDWord()
                fun getOpt opt conv v =
                    if Word.andb(Word.fromInt fields, Word.fromInt opt) = 0w0 then NONE else SOME(conv v)
                fun I x = x

                val orientation         = (getOpt DM_ORIENTATION toDMO o getShort) ()
                val paperSize           = (getOpt DM_PAPERSIZE toDMPS o getShort) ()
                val paperLength         = getOpt DM_PAPERLENGTH I (getShort())
                val paperWidth          = getOpt DM_PAPERWIDTH I (getShort())
                val scale               = getOpt DM_SCALE I (getShort())
                val copies              = getOpt DM_COPIES I (getShort())
                val defaultSource       = (getOpt DM_DEFAULTSOURCE toDMS o getShort) ()
                val printQuality        = (getOpt DM_PRINTQUALITY toDMR o getShort) ()
                val colour              = (getOpt DM_COLOR toDMC o getShort) ()
                val duplex              = (getOpt DM_DUPLEX toDMD o getShort) ()
                val yResolution         = getOpt DM_YRESOLUTION I (getShort())
                val ttOption            = (getOpt DM_TTOPTION toDMTT o getShort) ()
                val collate             = getOpt DM_COLLATE I (getShort())
                val formName            = getOpt DM_FORMNAME I (fromCstring(!ptr))
                val () = ptr := !ptr ++ 0w32
                val logPixels           = getOpt DM_LOGPIXELS I (getShort())
                val bitsPerPixel        = getOpt DM_BITSPERPEL I (getDWord())
                val pelsWidth           = getOpt DM_PELSWIDTH I (getDWord())
                val pelsHeight          = getOpt DM_PELSHEIGHT I (getDWord())
                val displayFlags        = getOpt DM_DISPLAYFLAGS I (getDWord()) (* Or dmNup *)
                val displayFrequency    = getOpt DM_DISPLAYFREQUENCY I (getDWord())
                val icmMethod           = (getOpt DM_ICMMETHOD toDMICMM o getDWord) ()
                val icmIntent           = (getOpt DM_ICMINTENT toDMICMI o getDWord) ()
                val mediaType           = (getOpt DM_MEDIATYPE toDMM o getDWord) ()
                val ditherType          = (getOpt DM_DITHERTYPE toDMDi o getDWord) ()
                val (*iccManufacturer*)_ = getDWord()
                val (*iccModel*)_       = getDWord()
                val panningWidth        = getOpt DM_PANNINGWIDTH I (getDWord())
                val panningHeight       = getOpt DM_PANNINGHEIGHT I (getDWord())
                val _ =
                    voidStar2Sysword(!ptr) - voidStar2Sysword v = Word.toLargeWord DMBaseSize orelse raise Fail "loadCDevMode: length wrong"
                (* There may be private data at the end. *)
                fun loadByte _ = Memory.get8(!ptr, 0w0) before ptr := !ptr ++ 0w1
                val driverPrivate = Word8Vector.tabulate(driverExtra, loadByte)
            in
                {
                deviceName = deviceName,
                driverVersion = driverVersion,
                orientation = orientation,
                paperSize = paperSize,
                paperLength = paperLength,
                paperWidth = paperWidth,
                scale = scale,
                copies = copies,
                defaultSource = defaultSource,
                printQuality = printQuality,
                color = colour,
                duplex = duplex,
                yResolution = yResolution,
                ttOption = ttOption,
                collate = case collate of NONE => NONE | SOME 0 => SOME false | SOME _ => SOME true,
                formName = formName,
                logPixels = logPixels,
                bitsPerPixel = bitsPerPixel,
                pelsWidth = pelsWidth,
                pelsHeight = pelsHeight,
                displayFlags = displayFlags,
                displayFrequency = displayFrequency,
                icmMethod = icmMethod,
                icmIntent = icmIntent,
                mediaType = mediaType,
                ditherType = ditherType,
                panningWidth = panningWidth,
                panningHeight = panningHeight,
                driverPrivate = driverPrivate
                }
            end

            fun setCDevMode(v: voidStar, (* This is the address of the data *)
            {
                deviceName: string,
                driverVersion: int,
                orientation: DMOrientation option,
                paperSize: DMPaperSize option,
                paperLength: int option,
                paperWidth: int option,
                scale: int option,
                copies: int option,
                defaultSource: DMSource option,
                printQuality: DMResolution option,
                color: DMColor option,
                duplex: DMDuplex option,
                yResolution: int option,
                ttOption: DMTrueType option,
                collate: bool option,
                formName: string option,
                logPixels: int option,
                bitsPerPixel: int option,
                pelsWidth: int option,
                pelsHeight: int option,
                displayFlags: int option, (* Apparently no longer used. *)
                displayFrequency: int option,
                icmMethod: DMICMMethod option,
                icmIntent: DMICMIntent option,
                mediaType: DMMedia option,
                ditherType: DMDither option,
                panningWidth: int option,
                panningHeight: int option,
                driverPrivate: Word8Vector.vector
                }: DEVMODE) : unit =
            let
                val ptr = ref v
                (* The name can be at most 31 characters. *)
                val devName =
                    if size deviceName > 31 then String.substring(deviceName, 0, 31) else deviceName
                (* setShort and setLong set the appropriate field and advance the pointer. *)
                fun setShort i = ignore(storeShort(!ptr, i)) before ptr := !ptr ++ sizeShort
                and setDWord i = ignore(storeDWord(!ptr, i)) before ptr := !ptr ++ sizeDWord

                (* Optional values default to zero.  If the option is SOME v we set the
                   appropriate bit in "fields". *)
                val fields = ref 0
                fun setOpt _ _ NONE = 0
                 |  setOpt opt conv (SOME v) = (fields := Word.toInt(Word.orb(Word.fromInt(!fields), Word.fromInt opt)); conv v)
                fun I x = x
                fun fromCollate true = 1 | fromCollate false = 0
                val form =
                    case formName of NONE => ""
                    |   SOME s => if size s > 31 then String.substring(s, 0, 31) else s
            in
                CharVector.appi(fn (i, c) => set8(!ptr, Word.fromInt i, Word8.fromInt(ord c))) devName;
                set8(!ptr, Word.fromInt(size devName), 0w0);
                ptr := !ptr ++ 0w32;
                setShort DM_SPECVERSION;
                setShort driverVersion;
                setShort (Word.toInt DMBaseSize);
                setShort (Word8Vector.length driverPrivate);
                setDWord 0; (* Fields - set this later. *)
                setShort(setOpt DM_ORIENTATION fromDMO orientation);
                setShort(setOpt DM_PAPERSIZE fromDMPS paperSize);
                setShort(setOpt DM_PAPERLENGTH I paperLength);
                setShort(setOpt DM_PAPERWIDTH I paperWidth);
                setShort(setOpt DM_SCALE I scale);
                setShort(setOpt DM_COPIES I copies);
                setShort(setOpt DM_DEFAULTSOURCE fromDMS defaultSource);
                setShort(setOpt DM_PRINTQUALITY fromDMR printQuality);
                setShort(setOpt DM_COLOR fromDMC color);
                setShort(setOpt DM_DUPLEX fromDMD duplex);
                setShort(setOpt DM_YRESOLUTION I yResolution);
                setShort(setOpt DM_TTOPTION fromDMTT ttOption);
                setShort(setOpt DM_COLLATE fromCollate collate);
                CharVector.appi(fn (i, c) => set8(!ptr, Word.fromInt i, Word8.fromInt(ord c))) form;
                set8(!ptr, Word.fromInt(size form), 0w0);
                ptr := !ptr ++ 0w32;
                setShort(setOpt DM_LOGPIXELS I logPixels);
                setDWord(setOpt DM_BITSPERPEL I bitsPerPixel);
                setDWord(setOpt DM_PELSWIDTH I pelsWidth);
                setDWord(setOpt DM_PELSHEIGHT I pelsHeight);
                setDWord(setOpt DM_DISPLAYFLAGS I displayFlags);
                setDWord(setOpt DM_DISPLAYFREQUENCY I displayFrequency);
                setDWord(setOpt DM_ICMMETHOD fromDMICMM icmMethod);
                setDWord(setOpt DM_ICMINTENT fromDMICMI icmIntent);
                setDWord(setOpt DM_MEDIATYPE fromDMM mediaType);
                setDWord(setOpt DM_DITHERTYPE fromDMDi ditherType);
                setDWord 0;
                setDWord 0;
                setDWord(setOpt DM_PANNINGWIDTH I panningWidth);
                setDWord(setOpt DM_PANNINGHEIGHT I panningHeight);

                (* Set the fields now. *)
                ignore(storeDWord(v ++ 0w40, !fields));

                let
                    fun copyToBuf (_, c) = set8(!ptr, 0w0, c) before ptr := !ptr ++ 0w1
                in
                    Word8Vector.appi copyToBuf driverPrivate
                end
            end
            
            fun devModeSize({driverPrivate: Word8Vector.vector, ...}: DEVMODE): word =
                DMBaseSize + Word.fromInt (Word8Vector.length driverPrivate)
                
            fun storeCDevMode(vaddr: voidStar, devmode) =
            let
                val v = malloc (devModeSize devmode)
                val () = setAddress(vaddr, 0w0, v)
            in
                setCDevMode(v, devmode);
                fn () => free v
            end
            
            fun loadCDevMode(vaddr: voidStar) : DEVMODE = getCDevMode(getAddress(vaddr, 0w0))
        in
            val LPDEVMODE =
                makeConversion{load=loadCDevMode, store=storeCDevMode, ctype=LowLevel.cTypePointer }
            val getCDevMode = getCDevMode
            and setCDevMode = setCDevMode
            and devModeSize = devModeSize
        end
    end
end;
