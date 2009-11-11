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

structure DeviceContext:
  sig
    type BITMAP and HDC and HGDIOBJ and HWND and HRGN
    type LOGBRUSH = Brush.LOGBRUSH
    type LOGFONT = Font.LOGFONT
    type LOGPEN = Pen.LOGPEN
    type POINT = {x: int, y: int}

    type StockObjectType
    val ANSI_FIXED_FONT : StockObjectType
    val ANSI_VAR_FONT : StockObjectType
    val BLACK_BRUSH : StockObjectType
    val BLACK_PEN : StockObjectType
    val CLR_INVALID : StockObjectType
    val DEFAULT_PALETTE : StockObjectType
    val DEVICE_DEFAULT_FONT : StockObjectType
    val DKGRAY_BRUSH : StockObjectType
    val GRAY_BRUSH : StockObjectType
    val HOLLOW_BRUSH : StockObjectType
    val LTGRAY_BRUSH : StockObjectType
    val NULL_BRUSH : StockObjectType
    val NULL_PEN : StockObjectType
    val OEM_FIXED_FONT : StockObjectType
    val SYSTEM_FIXED_FONT : StockObjectType
    val SYSTEM_FONT : StockObjectType
    val WHITE_BRUSH : StockObjectType
    val WHITE_PEN : StockObjectType

    val GetStockObject : StockObjectType -> HGDIOBJ

    eqtype DeviceItem
    val ASPECTX : DeviceItem
    val ASPECTXY : DeviceItem
    val ASPECTY : DeviceItem
    val BITSPIXEL : DeviceItem
    val CLIPCAPS : DeviceItem
    val COLORRES : DeviceItem
    val CURVECAPS : DeviceItem
    val DRIVERVERSION : DeviceItem
    val HORZRES : DeviceItem
    val HORZSIZE : DeviceItem
    val LINECAPS : DeviceItem
    val LOGPIXELSX : DeviceItem
    val LOGPIXELSY : DeviceItem
    val NUMBRUSHES : DeviceItem
    val NUMCOLORS : DeviceItem
    val NUMFONTS : DeviceItem
    val NUMMARKERS : DeviceItem
    val NUMPENS : DeviceItem
    val NUMRESERVED : DeviceItem
    val PDEVICESIZE : DeviceItem
    val PHYSICALHEIGHT : DeviceItem
    val PHYSICALOFFSETX : DeviceItem
    val PHYSICALOFFSETY : DeviceItem
    val PHYSICALWIDTH : DeviceItem
    val PLANES : DeviceItem
    val POLYGONALCAPS : DeviceItem
    val RASTERCAPS : DeviceItem
    val SCALINGFACTORX : DeviceItem
    val SCALINGFACTORY : DeviceItem
    val SIZEPALETTE : DeviceItem
    val TECHNOLOGY : DeviceItem
    val TEXTCAPS : DeviceItem
    val VERTRES : DeviceItem
    val VERTSIZE : DeviceItem

    val GetDeviceCaps : HDC * DeviceItem -> int

    (* Results of various calls to GetDeviceCaps.  Perhaps its result type should
       be a union. *)
    val CC_CHORD : int
    val CC_CIRCLES : int
    val CC_ELLIPSES : int
    val CC_INTERIORS : int
    val CC_NONE : int
    val CC_PIE : int
    val CC_ROUNDRECT : int
    val CC_STYLED : int
    val CC_WIDE : int
    val CC_WIDESTYLED : int

    val CP_NONE : int
    val CP_RECTANGLE : int
    val CP_REGION : int

    val DT_CHARSTREAM : int
    val DT_DISPFILE : int
    val DT_METAFILE : int
    val DT_PLOTTER : int
    val DT_RASCAMERA : int
    val DT_RASDISPLAY : int
    val DT_RASPRINTER : int

    val LC_INTERIORS : int
    val LC_MARKER : int
    val LC_NONE : int
    val LC_POLYLINE : int
    val LC_POLYMARKER : int
    val LC_STYLED : int
    val LC_WIDE : int
    val LC_WIDESTYLED : int

    val PC_INTERIORS : int
    val PC_NONE : int
    val PC_PATHS : int
    val PC_POLYGON : int
    val PC_POLYPOLYGON : int
    val PC_RECTANGLE : int
    val PC_SCANLINE : int
    val PC_STYLED : int
    val PC_TRAPEZOID : int
    val PC_WIDE : int
    val PC_WIDESTYLED : int
    val PC_WINDPOLYGON : int

    val RC_BANDING : int
    val RC_BIGFONT : int
    val RC_BITBLT : int
    val RC_BITMAP64 : int
    val RC_DEVBITS : int
    val RC_DIBTODEV : int
    val RC_DI_BITMAP : int
    val RC_FLOODFILL : int
    val RC_GDI20_OUTPUT : int
    val RC_GDI20_STATE : int
    val RC_OP_DX_OUTPUT : int
    val RC_PALETTE : int
    val RC_SAVEBITMAP : int
    val RC_SCALING : int
    val RC_STRETCHBLT : int
    val RC_STRETCHDIB : int

    val TC_CP_STROKE : int
    val TC_CR_90 : int
    val TC_CR_ANY : int
    val TC_EA_DOUBLE : int
    val TC_IA_ABLE : int
    val TC_OP_CHARACTER : int
    val TC_OP_STROKE : int
    val TC_RA_ABLE : int
    val TC_RESERVED : int
    val TC_SA_CONTIN : int
    val TC_SA_DOUBLE : int
    val TC_SA_INTEGER : int
    val TC_SCROLLBLT : int
    val TC_SF_X_YINDEP : int
    val TC_SO_ABLE : int
    val TC_UA_ABLE : int
    val TC_VA_ABLE : int

    datatype DMColor = DMCOLOR_COLOR | DMCOLOR_MONOCHROME
    and DMDither =
          DMDITHER_COARSE
        | DMDITHER_FINE
        | DMDITHER_GRAYSCALE
        | DMDITHER_LINEART
        | DMDITHER_NONE
        | DMDITHER_OTHER of int
    and DMDuplex = DMDUP_HORIZONTAL | DMDUP_SIMPLEX | DMDUP_VERTICAL
    and DMICMIntent =
          DMICMINTENT_OTHER of int
        | DMICM_COLORMETRIC
        | DMICM_CONTRAST
        | DMICM_SATURATE
    and DMICMMethod =
          DMICMMETHOD_DEVICE
        | DMICMMETHOD_DRIVER
        | DMICMMETHOD_NONE
        | DMICMMETHOD_OTHER of int
        | DMICMMETHOD_SYSTEM
    and DMMedia =
          DMICMMEDIA_OTHER of int
        | DMMEDIA_GLOSSY
        | DMMEDIA_STANDARD
        | DMMEDIA_TRANSPARENCY
    and DMOrientation = DMORIENT_LANDSCAPE | DMORIENT_PORTRAIT
    and DMPaperSize =
          DMPAPER_10X11
        | DMPAPER_10X14
        | DMPAPER_11X17
        | DMPAPER_15X11
        | DMPAPER_9X11
        | DMPAPER_A2
        | DMPAPER_A3
        | DMPAPER_A3_EXTRA
        | DMPAPER_A3_EXTRA_TRANSVERSE
        | DMPAPER_A3_TRANSVERSE
        | DMPAPER_A4
        | DMPAPER_A4SMALL
        | DMPAPER_A4_EXTRA
        | DMPAPER_A4_PLUS
        | DMPAPER_A4_TRANSVERSE
        | DMPAPER_A5
        | DMPAPER_A5_EXTRA
        | DMPAPER_A5_TRANSVERSE
        | DMPAPER_A_PLUS
        | DMPAPER_B4
        | DMPAPER_B5
        | DMPAPER_B5_EXTRA
        | DMPAPER_B5_TRANSVERSE
        | DMPAPER_B_PLUS
        | DMPAPER_CSHEET
        | DMPAPER_DSHEET
        | DMPAPER_ENV_10
        | DMPAPER_ENV_11
        | DMPAPER_ENV_12
        | DMPAPER_ENV_14
        | DMPAPER_ENV_9
        | DMPAPER_ENV_B4
        | DMPAPER_ENV_B5
        | DMPAPER_ENV_B6
        | DMPAPER_ENV_C3
        | DMPAPER_ENV_C4
        | DMPAPER_ENV_C5
        | DMPAPER_ENV_C6
        | DMPAPER_ENV_C65
        | DMPAPER_ENV_DL
        | DMPAPER_ENV_INVITE
        | DMPAPER_ENV_ITALY
        | DMPAPER_ENV_MONARCH
        | DMPAPER_ENV_PERSONAL
        | DMPAPER_ESHEET
        | DMPAPER_EXECUTIVE
        | DMPAPER_FANFOLD_LGL_GERMAN
        | DMPAPER_FANFOLD_STD_GERMAN
        | DMPAPER_FANFOLD_US
        | DMPAPER_FOLIO
        | DMPAPER_ISO_B4
        | DMPAPER_JAPANESE_POSTCARD
        | DMPAPER_LEDGER
        | DMPAPER_LEGAL
        | DMPAPER_LEGAL_EXTRA
        | DMPAPER_LETTER
        | DMPAPER_LETTERSMALL
        | DMPAPER_LETTER_EXTRA
        | DMPAPER_LETTER_EXTRA_TRANSVERSE
        | DMPAPER_LETTER_PLUS
        | DMPAPER_LETTER_TRANSVERSE
        | DMPAPER_NOTE
        | DMPAPER_OTHER of int
        | DMPAPER_QUARTO
        | DMPAPER_RESERVED_48
        | DMPAPER_RESERVED_49
        | DMPAPER_STATEMENT
        | DMPAPER_TABLOID
        | DMPAPER_TABLOID_EXTRA
    and DMResolution =
          DMRES_DPI of int
        | DMRES_DRAFT
        | DMRES_HIGH
        | DMRES_LOW
        | DMRES_MEDIUM
    and DMSource =
          DMBIN_AUTO
        | DMBIN_CASSETTE
        | DMBIN_ENVELOPE
        | DMBIN_ENVMANUAL
        | DMBIN_FORMSOURCE
        | DMBIN_LARGECAPACITY
        | DMBIN_LARGEFMT
        | DMBIN_LOWER
        | DMBIN_MANUAL
        | DMBIN_MIDDLE
        | DMBIN_ONLYONE
        | DMBIN_SMALLFMT
        | DMBIN_TRACTOR
        | DMBIN_UPPER
        | DMSOURCE_OTHER of int
    and DMTrueType =
          DMTT_BITMAP
        | DMTT_DOWNLOAD
        | DMTT_DOWNLOAD_OUTLINE
        | DMTT_SUBDEV

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

    val CancelDC : HDC -> unit
    val CreateCompatibleDC : HDC -> HDC
    val CreateDC : string option * string option * string option * DEVMODE option -> HDC

    val DeleteDC : HDC -> unit
    val DeleteObject : HGDIOBJ -> unit

    datatype
      EnumObject =
          OBJ_BITMAP
        | OBJ_BRUSH
        | OBJ_DC
        | OBJ_ENHMETADC
        | OBJ_ENHMETAFILE
        | OBJ_EXTPEN
        | OBJ_FONT
        | OBJ_MEMDC
        | OBJ_METADC
        | OBJ_METAFILE
        | OBJ_PAL
        | OBJ_PEN
        | OBJ_REGION
    val GetCurrentObject : HDC * EnumObject -> HGDIOBJ
    val GetDC : HWND -> HDC

    datatype
      DeviceContextFlag =
          DCX_CACHE
        | DCX_CLIPCHILDREN
        | DCX_CLIPSIBLINGS
        | DCX_EXCLUDERGN
        | DCX_EXCLUDEUPDATE
        | DCX_INTERSECTRGN
        | DCX_INTERSECTUPDATE
        | DCX_LOCKWINDOWUPDATE
        | DCX_NORECOMPUTE
        | DCX_NORESETATTRS
        | DCX_PARENTCLIP
        | DCX_VALIDATE
        | DCX_WINDOW

    val GetDCEx : HWND * HRGN * DeviceContextFlag list -> HDC
    val GetDCOrgEx : HDC -> POINT

    datatype
      GetObject =
          GO_Bitmap of BITMAP
        | GO_Brush of LOGBRUSH
        | GO_Font of LOGFONT
        | GO_Palette of int
        | GO_Pen of LOGPEN

    val GetObject : HGDIOBJ -> GetObject

    val GetObjectType : HGDIOBJ -> EnumObject


    val ReleaseDC : HWND * HDC -> bool
    val ResetDC : HDC * DEVMODE -> HDC
    val RestoreDC : HDC * int -> unit
    val SaveDC : HDC -> int
    val SelectObject : HDC * HGDIOBJ -> HGDIOBJ

    type DEVNAMES = {driver: string, device: string, output: string, default: bool}
  end
 =
struct
    local
        open CInterface Base

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

        fun checkDC c = (checkResult(not(isHdcNull c)); c)
    in
        type HDC = HDC and HGDIOBJ = HGDIOBJ and HWND = HWND and HRGN = HRGN
        type LOGFONT = Font.LOGFONT

        open GdiBase DeviceBase

        type POINT = POINT

        datatype DeviceContextFlag =
            DCX_WINDOW | DCX_CACHE | DCX_NORESETATTRS | DCX_CLIPCHILDREN | DCX_CLIPSIBLINGS |
            DCX_PARENTCLIP | DCX_EXCLUDERGN | DCX_INTERSECTRGN | DCX_EXCLUDEUPDATE | DCX_INTERSECTUPDATE |
            DCX_LOCKWINDOWUPDATE | DCX_NORECOMPUTE | DCX_VALIDATE
        local
            val tab = [
                (DCX_WINDOW,            0x00000001),
                (DCX_CACHE,             0x00000002),
                (DCX_NORESETATTRS,      0x00000004),
                (DCX_CLIPCHILDREN,      0x00000008),
                (DCX_CLIPSIBLINGS,      0x00000010),
                (DCX_PARENTCLIP,        0x00000020),
                (DCX_EXCLUDERGN,        0x00000040),
                (DCX_INTERSECTRGN,      0x00000080),
                (DCX_EXCLUDEUPDATE,     0x00000100),
                (DCX_INTERSECTUPDATE,   0x00000200),
                (DCX_LOCKWINDOWUPDATE,  0x00000400),
                (DCX_NORECOMPUTE,       0x00100000),
                (DCX_VALIDATE,          0x00200000)]
        in
            val DEVICECONTEXTFLAG = tableSetConversion(tab, NONE)
        end


        (* DEVNAMES is not actually used in this structure. *)
        type DEVNAMES = {driver: string, device: string, output: string, default: bool}

        datatype EnumObject = OBJ_PEN | OBJ_BRUSH | OBJ_DC | OBJ_METADC | OBJ_PAL | OBJ_FONT |
            OBJ_BITMAP | OBJ_REGION | OBJ_METAFILE | OBJ_MEMDC | OBJ_EXTPEN | OBJ_ENHMETADC |
            OBJ_ENHMETAFILE

        local
            val tab = [
                (OBJ_PEN,                                      1),
                (OBJ_BRUSH,                                    2),
                (OBJ_DC,                                       3),
                (OBJ_METADC,                                   4),
                (OBJ_PAL,                                      5),
                (OBJ_FONT,                                     6),
                (OBJ_BITMAP,                                   7),
                (OBJ_REGION,                                   8),
                (OBJ_METAFILE,                                 9),
                (OBJ_MEMDC,                                    10),
                (OBJ_EXTPEN,                                   11),
                (OBJ_ENHMETADC,                                12),
                (OBJ_ENHMETAFILE,                              13)
            ]
            datatype EnumObject =
            W of int
            (* GetObjectType returns 0 in the event of an error. *)
            fun toInt _ = raise Match
            fun fromInt i = (checkResult(i <> 0); raise Match);
        in
            val ENUMOBJECT = tableConversion(tab, SOME(fromInt, toInt))
        end

        local
            datatype DeviceItem =
            W of int
        in
            type DeviceItem = DeviceItem
            val DEVICEITEM = absConversion {abs = W, rep = fn W n => n} INT
        
            val DRIVERVERSION                                = W (0 (* Device driver version *))
            val TECHNOLOGY                                   = W (2 (* Device classification *))
            val HORZSIZE                                     = W (4 (* Horizontal size in millimeters *))
            val VERTSIZE                                     = W (6 (* Vertical size in millimeters *))
            val HORZRES                                      = W (8 (* Horizontal width in pixels *))
            val VERTRES                                      = W (10 (* Vertical width in pixels *))
            val BITSPIXEL                                    = W (12 (* Number of bits per pixel *))
            val PLANES                                       = W (14 (* Number of planes *))
            val NUMBRUSHES                                   = W (16 (* Number of brushes the device has *))
            val NUMPENS                                      = W (18 (* Number of pens the device has *))
            val NUMMARKERS                                   = W (20 (* Number of markers the device has *))
            val NUMFONTS                                     = W (22 (* Number of fonts the device has *))
            val NUMCOLORS                                    = W (24 (* Number of colors the device supports *))
            val PDEVICESIZE                                  = W (26 (* Size required for device descriptor *))
            val CURVECAPS                                    = W (28 (* Curve capabilities *))
            val LINECAPS                                     = W (30 (* Line capabilities *))
            val POLYGONALCAPS                                = W (32 (* Polygonal capabilities *))
            val TEXTCAPS                                     = W (34 (* Text capabilities *))
            val CLIPCAPS                                     = W (36 (* Clipping capabilities *))
            val RASTERCAPS                                   = W (38 (* Bitblt capabilities *))
            val ASPECTX                                      = W (40 (* Length of the X leg *))
            val ASPECTY                                      = W (42 (* Length of the Y leg *))
            val ASPECTXY                                     = W (44 (* Length of the hypotenuse *))
            val LOGPIXELSX                                   = W (88 (* Logical pixels/inch in X *))
            val LOGPIXELSY                                   = W (90 (* Logical pixels/inch in Y *))
            val SIZEPALETTE                                  = W (104 (* Number of entries in physical palette *))
            val NUMRESERVED                                  = W (106 (* Number of reserved entries in palette *))
            val COLORRES                                     = W (108 (* Actual color resolution *))
            val PHYSICALWIDTH                                = W (110 (* Physical Width in device units *))
            val PHYSICALHEIGHT                               = W (111 (* Physical Height in device units *))
            val PHYSICALOFFSETX                              = W (112 (* Physical Printable Area x margin *))
            val PHYSICALOFFSETY                              = W (113 (* Physical Printable Area y margin *))
            val SCALINGFACTORX                               = W (114 (* Scaling factor x *))
            val SCALINGFACTORY                               = W (115 (* Scaling factor y *))
        end

        (* Results of GetDeviceCaps.  Since it returns an int all these are simply ints. *)

        val DT_PLOTTER          = 0   (* Vector plotter                   *)
        val DT_RASDISPLAY       = 1   (* Raster display                   *)
        val DT_RASPRINTER       = 2   (* Raster printer                   *)
        val DT_RASCAMERA        = 3   (* Raster camera                    *)
        val DT_CHARSTREAM       = 4   (* Character-stream, PLP            *)
        val DT_METAFILE         = 5   (* Metafile, VDM                    *)
        val DT_DISPFILE         = 6   (* Display-file                     *)

        (* Curve Capabilities *)
        val CC_NONE             = 0   (* Curves not supported             *)
        val CC_CIRCLES          = 1   (* Can do circles                   *)
        val CC_PIE              = 2   (* Can do pie wedges                *)
        val CC_CHORD            = 4   (* Can do chord arcs                *)
        val CC_ELLIPSES         = 8   (* Can do ellipese                  *)
        val CC_WIDE             = 16  (* Can do wide lines                *)
        val CC_STYLED           = 32  (* Can do styled lines              *)
        val CC_WIDESTYLED       = 64  (* Can do wide styled lines         *)
        val CC_INTERIORS        = 128 (* Can do interiors                 *)
        val CC_ROUNDRECT        = 256 (*                                  *)

        (* Line Capabilities *)
        val LC_NONE             = 0   (* Lines not supported              *)
        val LC_POLYLINE         = 2   (* Can do polylines                 *)
        val LC_MARKER           = 4   (* Can do markers                   *)
        val LC_POLYMARKER       = 8   (* Can do polymarkers               *)
        val LC_WIDE             = 16  (* Can do wide lines                *)
        val LC_STYLED           = 32  (* Can do styled lines              *)
        val LC_WIDESTYLED       = 64  (* Can do wide styled lines         *)
        val LC_INTERIORS        = 128 (* Can do interiors                 *)

        (* Polygonal Capabilities *)
        val PC_NONE             = 0   (* Polygonals not supported         *)
        val PC_POLYGON          = 1   (* Can do polygons                  *)
        val PC_RECTANGLE        = 2   (* Can do rectangles                *)
        val PC_WINDPOLYGON      = 4   (* Can do winding polygons          *)
        val PC_TRAPEZOID        = 4   (* Can do trapezoids                *)
        val PC_SCANLINE         = 8   (* Can do scanlines                 *)
        val PC_WIDE             = 16  (* Can do wide borders              *)
        val PC_STYLED           = 32  (* Can do styled borders            *)
        val PC_WIDESTYLED       = 64  (* Can do wide styled borders       *)
        val PC_INTERIORS        = 128 (* Can do interiors                 *)
        val PC_POLYPOLYGON      = 256 (* Can do polypolygons              *)
        val PC_PATHS            = 512 (* Can do paths                     *)

        (* Clipping Capabilities *)
        val CP_NONE             = 0   (* No clipping of output            *)
        val CP_RECTANGLE        = 1   (* Output clipped to rects          *)
        val CP_REGION           = 2   (* obsolete                         *)

        (* Text Capabilities *)
        val TC_OP_CHARACTER     = 0x00000001  (* Can do OutputPrecision   CHARACTER      *)
        val TC_OP_STROKE        = 0x00000002  (* Can do OutputPrecision   STROKE         *)
        val TC_CP_STROKE        = 0x00000004  (* Can do ClipPrecision     STROKE         *)
        val TC_CR_90            = 0x00000008  (* Can do CharRotAbility    90             *)
        val TC_CR_ANY           = 0x00000010  (* Can do CharRotAbility    ANY            *)
        val TC_SF_X_YINDEP      = 0x00000020  (* Can do ScaleFreedom      X_YINDEPENDENT *)
        val TC_SA_DOUBLE        = 0x00000040  (* Can do ScaleAbility      DOUBLE         *)
        val TC_SA_INTEGER       = 0x00000080  (* Can do ScaleAbility      INTEGER        *)
        val TC_SA_CONTIN        = 0x00000100  (* Can do ScaleAbility      CONTINUOUS     *)
        val TC_EA_DOUBLE        = 0x00000200  (* Can do EmboldenAbility   DOUBLE         *)
        val TC_IA_ABLE          = 0x00000400  (* Can do ItalisizeAbility  ABLE           *)
        val TC_UA_ABLE          = 0x00000800  (* Can do UnderlineAbility  ABLE           *)
        val TC_SO_ABLE          = 0x00001000  (* Can do StrikeOutAbility  ABLE           *)
        val TC_RA_ABLE          = 0x00002000  (* Can do RasterFontAble    ABLE           *)
        val TC_VA_ABLE          = 0x00004000  (* Can do VectorFontAble    ABLE           *)
        val TC_RESERVED         = 0x00008000
        val TC_SCROLLBLT        = 0x00010000  (* Don't do text scroll with blt           *)

        (* Raster Capabilities *)
        val RC_BITBLT           = 1       (* Can do standard BLT.             *)
        val RC_BANDING          = 2       (* Device requires banding support  *)
        val RC_SCALING          = 4       (* Device requires scaling support  *)
        val RC_BITMAP64         = 8       (* Device can support >64K bitmap   *)
        val RC_GDI20_OUTPUT     = 0x0010      (* has 2.0 output calls         *)
        val RC_GDI20_STATE      = 0x0020
        val RC_SAVEBITMAP       = 0x0040
        val RC_DI_BITMAP        = 0x0080      (* supports DIB to memory       *)
        val RC_PALETTE          = 0x0100      (* supports a palette           *)
        val RC_DIBTODEV         = 0x0200      (* supports DIBitsToDevice      *)
        val RC_BIGFONT          = 0x0400      (* supports >64K fonts          *)
        val RC_STRETCHBLT       = 0x0800      (* supports StretchBlt          *)
        val RC_FLOODFILL        = 0x1000      (* supports FloodFill           *)
        val RC_STRETCHDIB       = 0x2000      (* supports StretchDIBits       *)
        val RC_OP_DX_OUTPUT     = 0x4000
        val RC_DEVBITS          = 0x8000

        local
            datatype StockObjectType =
            W of int
        in
            type StockObjectType = StockObjectType
            val STOCKOBJECTTYPE  = absConversion {abs = W, rep = fn W n => n} INT
        
            val WHITE_BRUSH                                  = W (0)
            val LTGRAY_BRUSH                                 = W (1)
            val GRAY_BRUSH                                   = W (2)
            val DKGRAY_BRUSH                                 = W (3)
            val BLACK_BRUSH                                  = W (4)
            val NULL_BRUSH                                   = W (5)
            val HOLLOW_BRUSH                                 = NULL_BRUSH
            val WHITE_PEN                                    = W (6)
            val BLACK_PEN                                    = W (7)
            val NULL_PEN                                     = W (8)
            val OEM_FIXED_FONT                               = W (10)
            val ANSI_FIXED_FONT                              = W (11)
            val ANSI_VAR_FONT                                = W (12)
            val SYSTEM_FONT                                  = W (13)
            val DEVICE_DEFAULT_FONT                          = W (14)
            val DEFAULT_PALETTE                              = W (15)
            val SYSTEM_FIXED_FONT                            = W (16)
            val STOCK_LAST                                   = W (16)
            val CLR_INVALID                                  = W (0xFFFFFFFF)
        end

        val CancelDC                   = call1(gdi "CancelDC") (HDC) (SUCCESSSTATE "CancelDC")
        val CreateCompatibleDC         = call1(gdi "CreateCompatibleDC") (HDC) HDC
        val DeleteDC                   = call1(gdi "DeleteDC") (HDC) (SUCCESSSTATE "DeleteDC")
        val DeleteObject               = call1(gdi "DeleteObject") (HGDIOBJ) (SUCCESSSTATE "DeleteObject")
        val GetCurrentObject           = call2(gdi "GetCurrentObject") (HDC,ENUMOBJECT) HGDIOBJ
        val GetDC                      = checkDC o call1(user "GetDC") (HWND) HDC
        val GetDCEx                    = checkDC o call3(user "GetDCEx") (HWND,HRGN,DEVICECONTEXTFLAG) HDC
        val GetDCOrgEx                 = gdicall_IW "GetDCOrgEx" (SUCCESSSTATE "GetDCOrgEx") (HDC,POINT)
        val GetDeviceCaps              = call2(gdi "GetDeviceCaps") (HDC,DEVICEITEM) INT
        val GetObjectType              = call1(gdi "GetObjectType") (HGDIOBJ) ENUMOBJECT
        val GetStockObject             = call1 (gdi "GetStockObject") (STOCKOBJECTTYPE) HGDIOBJ
        val ReleaseDC                  = call2(user "ReleaseDC") (HWND,HDC) BOOL
        val RestoreDC                  = call2(gdi "RestoreDC") (HDC,INT) (SUCCESSSTATE "RestoreDC")
        val SaveDC                     = call1(gdi "SaveDC") (HDC) INT
        val ResetDC                    = call2 (gdi "ResetDC") (HDC, LPDEVMODE) HDC
        (* The result of SelectObject is a bit of a mess.  It is the original object being
           replaced except if the argument is a region when it returns a RESULTREGION.
           Perhaps we need a different function for that. *)
        val SelectObject               = call2(gdi "SelectObject") (HDC,HGDIOBJ) HGDIOBJ

        local
            val cdc = call4 (gdi "CreateDCA") (STRINGOPT, STRINGOPT, STRINGOPT, POINTER) HDC
            val (_, toCdev, _) = breakConversion LPDEVMODE
        in
            fun CreateDC(driver: string option, device: string option, output: string option,
                    data: DEVMODE option) =
                checkDC
                    (cdc(driver, device, output,
                    case data of NONE => toCint 0 | SOME dev => toCdev dev))
        end

        (* GetObject returns information about different kinds of GDI object.
           It takes a pointer to a structure whose size and format differ according
           to the type of object.  To implement this properly in ML we have to
           find out the type before we start. *)
        datatype GetObject =
            GO_Bitmap of BITMAP
        (*| GO_DIBSection of DIBSECTION*) (* This is a subset of BITMAP *)
        (*| GO_ExPen of EXTLOGPEN*)
        |   GO_Brush of LOGBRUSH
        |   GO_Font of LOGFONT
        |   GO_Pen of LOGPEN
        |   GO_Palette of int
        local
            val getObj = call3 (gdi "GetObjectA") (HGDIOBJ, INT, POINTER) INT
            val (fromCBM, _, _) = breakConversion BITMAP
            val (fromCLF, _, _) = breakConversion FontBase.LOGFONT
            val (fromCLB, _, _) = breakConversion LOGBRUSH
            val (fromCLP, _, _) = breakConversion LOGPEN
        in
            fun GetObject(hgdi: HGDIOBJ): GetObject =
            let
                (* Call with a NULL buffer to find out the memory required.  Also
                   checks the GDI object. *)
                val space = getObj(hgdi, 0, toCint 0)
                val _ = checkResult(space > 0);
                val mem = alloc space Cchar
                val res = getObj(hgdi, space, address mem)
            in
                case GetObjectType hgdi of
                    OBJ_PEN     => GO_Pen(fromCLP mem)
                |   OBJ_BRUSH   => GO_Brush(fromCLB mem)
                |   OBJ_BITMAP  => GO_Bitmap(fromCBM mem)
                |   OBJ_FONT    => GO_Font(fromCLF mem)
                (*| OBJ_EXPEN   => *) (* TODO!!*)
                |   OBJ_PAL     => GO_Palette(fromCshort mem) (* Number of entries. *)
                |   _ => raise Fail "Different type"
            end
        end

        (*
            Other Device context functions:
                ChangeDisplaySettings  
                ChangeDisplaySettingsEx  
                CreateIC  
                DeviceCapabilities  
                DrawEscape  
                EnumDisplayDevices  
                EnumDisplaySettings  
                EnumObjects  
                EnumObjectsProc  
                GetDCBrushColor - NT 5.0 and Win 98 only
                GetDCPenColor   - NT 5.0 and Win 98 only
                SetDCBrushColor - NT 5.0 and Win 98 only
                SetDCPenColor   - NT 5.0 and Win 98 only
        *)
    end
end;
