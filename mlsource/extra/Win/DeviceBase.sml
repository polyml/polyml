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

structure DeviceBase =
struct
    local
        open CInterface Base
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
            iccManufacturer: int option,
            iccModel: int option,
            panningWidth: int option,
            panningHeight: int option,
            driverPrivate: Word8Vector.vector
            }

        local
            val DM_SPECVERSION = 0x0401
            val DMBaseSize = 156 (* Size of structure without any user data. *)

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
            val DM_ICCMANUFACTURER  = 0x20000000
            val DM_ICCMODEL         = 0x40000000

            (* The argument "vaddr" is the address of the structure.
               Perhaps we shouldn't do it that way and instead use POINTERTO
               in various places. *)
            fun fromCDevMode vaddr : DEVMODE =
            let
                val v = deref vaddr
                val off = ref 0
                fun getShort() =
                let
                    val r = fromCshort(offset (!off) Cchar v)
                in
                    off := !off + sizeof(Cshort);
                    r
                end;
                fun getLong() =
                let
                    val r = fromClong(offset (!off) Cchar v)
                in
                    off := !off + sizeof(Clong);
                    r
                end;
                val deviceName = fromCstring(address v)
                val _ = off := !off + (32 * sizeof(Cchar))
                val specVersion         = getShort()
                val driverVersion       = getShort()
                val size                = getShort()
                val driverExtra         = getShort()
                (* The "fields" value determines which of the fields are valid. *)
                val fields              = getLong()
                fun getOpt opt conv v =
                    if IntInf.andb(fields, opt) = 0 then NONE else SOME(conv v)
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
                val formName            = getOpt DM_FORMNAME I (fromCstring(address(offset (!off) Cchar v)))
                val _ = off := !off + (32 * sizeof(Cchar))
                val logPixels           = getOpt DM_LOGPIXELS I (getShort())
                val bitsPerPixel        = getOpt DM_BITSPERPEL I (getLong())
                val pelsWidth           = getOpt DM_PELSWIDTH I (getLong())
                val pelsHeight          = getOpt DM_PELSHEIGHT I (getLong())
                val displayFlags        = getOpt DM_DISPLAYFLAGS I (getLong())
                val displayFrequency    = getOpt DM_DISPLAYFREQUENCY I (getLong())
                val icmMethod           = (getOpt DM_ICMMETHOD toDMICMM o getLong) ()
                val icmIntent           = (getOpt DM_ICMINTENT toDMICMI o getLong) ()
                val mediaType           = (getOpt DM_MEDIATYPE toDMM o getLong) ()
                val ditherType          = (getOpt DM_DITHERTYPE toDMDi o getLong) ()
                val iccManufacturer     = getOpt DM_ICCMANUFACTURER I (getLong())
                val iccModel            = getOpt DM_ICCMODEL I (getLong())
                val panningWidth        = getOpt DM_PANNINGWIDTH I (getLong())
                val panningHeight       = getOpt DM_PANNINGHEIGHT I (getLong())
                (* There may be private data at the end. *)
                val driverPrivate = toWord8vec(address(offset size Cchar v), driverExtra) 
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
                iccManufacturer = iccManufacturer,
                iccModel = iccModel,
                panningWidth = panningWidth,
                panningHeight = panningHeight,
                driverPrivate = driverPrivate
                }
            end

            fun toCDevMode({
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
                iccManufacturer: int option,
                iccModel: int option,
                panningWidth: int option,
                panningHeight: int option,
                driverPrivate: Word8Vector.vector
                }: DEVMODE) : vol =
            let
                val driverExtra = Word8Vector.length driverPrivate
                val v = alloc (DMBaseSize + driverExtra) Cchar
                val off = ref 0
                (* The name can be at most 31 characters. *)
                val devName =
                    if size deviceName > 31 then String.substring(deviceName, 0, 31) else deviceName
                (* setShort and setLong set the appropriate field and advance the pointer. *)
                fun setShort i =
                    (assign Cshort (offset (!off) Cchar v) (toCshort i); off := !off + sizeof Cshort)
                fun setLong i =
                    (assign Clong (offset (!off) Cchar v) (toClong i); off := !off + sizeof Clong)

                (* Optional values default to zero.  If the option is SOME v we set the
                   appropriate bit in "fields". *)
                val fields = ref 0
                fun setOpt opt _ NONE = 0
                 |  setOpt opt conv (SOME v) = (fields := IntInf.orb(!fields, opt); conv v)
                fun I x = x
                fun fromCollate true = 1 | fromCollate false = 0
                val form =
                    case formName of NONE => ""
                    |   SOME s => if size s > 31 then String.substring(s, 0, 31) else s
            in
                fillCstring v devName; off := !off + 32;
                setShort DM_SPECVERSION;
                setShort driverVersion;
                setShort DMBaseSize;
                setShort driverExtra;
                setLong 0; (* Fields - set this later. *)
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
                fillCstring (offset (!off) Cchar v) form; off := !off + 32;
                setShort(setOpt DM_LOGPIXELS I logPixels);
                setLong(setOpt DM_BITSPERPEL I bitsPerPixel);
                setLong(setOpt DM_PELSWIDTH I pelsWidth);
                setLong(setOpt DM_PELSHEIGHT I pelsHeight);
                setLong(setOpt DM_DISPLAYFLAGS I displayFlags);
                setLong(setOpt DM_DISPLAYFREQUENCY I displayFrequency);
                setLong(setOpt DM_ICMMETHOD fromDMICMM icmMethod);
                setLong(setOpt DM_ICMINTENT fromDMICMI icmIntent);
                setLong(setOpt DM_MEDIATYPE fromDMM mediaType);
                setLong(setOpt DM_DITHERTYPE fromDMDi ditherType);
                setLong(setOpt DM_ICCMANUFACTURER I iccManufacturer);
                setLong(setOpt DM_ICCMODEL I iccModel);
                setLong(setOpt DM_PANNINGWIDTH I panningWidth);
                setLong(setOpt DM_PANNINGHEIGHT I panningHeight);

                (* Set the fields now. *)
                assign Clong (offset 40 Cchar v) (toClong(!fields));

                let
                    fun copyToBuf (i, c): unit =
                        assign Cchar (offset (i + !off) Cchar v) (toCint(Word8.toInt c))
                in
                    Word8Vector.appi copyToBuf driverPrivate
                end;

                address v
            end
        in
            val LPDEVMODE = mkConversion fromCDevMode toCDevMode (Cpointer Cvoid)
        end
    end
end;
