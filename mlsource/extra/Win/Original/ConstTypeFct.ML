(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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


(*------------------------------------------------------------------------------
CONTENTS: 3 MODE=ML SEP=3
  183.   import Macro
  194.   functor
  208.   misc
  216.   'a encoding wraps 'a <-> int
  265.   DocumentMode
  292.   DeviceCapability
  329.   RasterOperation
  366.   RasterOpCode
  401.   GdiError
  419.   ResultRegion
  455.   RegionOperation
  487.   StretchMode
  510.   PolyFillMode
  535.   TextAlign
  563.   VirtualTextAlign
  586.   ExtendedTextOut
  619.   DCBound
  646.   MetaFunction
  732.   GDIEscapes
  823.   SpoolerErrorCode
  853.   EnumObject
  885.   XFormType
  908.   BitmapCompression
  931.   TextMetricPitchandFamily
  953.   NewTextMetric
  972.   LogicalFont
  990.   OutputPrecision
  1017.  ClippingPrecision
  1043.  OutputQuality
  1064.  FontPitch
  1085.  CharacterSet
  1110.  FontFamily
  1134.  FontWeight
  1167.  CulturePANOSE
  1187.  FamilyTypePANOSE
  1209.  IndexPANOSE
  1236.  SerifStylePANOSE
  1268.  WeightPANOSE
  1296.  ProportionPANOSE
  1322.  ContrastPANOSE
  1348.  StrokeVariationPANOSE
  1373.  ArmStylePANOSE
  1401.  LetterFormPANOSE
  1433.  MidlinePANOSE
  1463.  XHeightPANOSE
  1487.  ExtLogFont
  1506.  FontType
  1525.  PaletteEntryFlag
  1546.  BkMode
  1568.  GraphicsMode
  1590.  PointType
  1613.  MapMode
  1643.  TransMode
  1661.  StockObjectType
  1698.  BrushStyle
  1724.  HatchStyle
  1766.  PenStyle
  1806.  ArcDirection
  1815.  DeviceItem
  1867.  DeviceTechnology
  1892.  CurveCapability
  1920.  LineCapability
  1947.  PolygonalCapability
  1975.  ClippingCapability
  1997.  TextCapability
  2033.  RasterCapability
  2067.  DeviceIndependentPalette
  2098.  SystemPaletteUse
  2120.  BitmapInit
  2141.  FloodFillMode
  2163.  DeviceMode
  2194.  PaperOrientation
  2212.  PaperSize
  2274.  BinMode
  2306.  DevModeResolution
  2361.  DocumentMode
  2387.  DeviceCapability
  2578.  misc C -> ML
  2589.  ResourceType
  2627.  ScrollBarType
  2664.  ShowWindowMode
  2706.  KeyFunction
  2728.  VirtualKey
  2833.  WindowHook
  2866.  HookCode
  2889.  HookProcedureCode
  2917.  InputEventType
  2955.  KeyLayoutFlag
  2979.  DesktopOperation
  3003.  WindowStationAccess
  3028.  GetWindowLongParam
  3054.  GetWindowLongValue
  3083.  WindowActivationState
  3104.  PowerState
  3127.  WindowMessage
  3306.  HitTest
  3350.  SendMessageTimeOut
  3369.  MessageActivateFlag
  3391.  SizeFlag
  3419.  WindowAttribute
  3443.  MouseKey
  3469.  WindowStyle
  3523.  WindowStyleExtended
  3546.  ClassStyle
  3577.  ClipboardFormat
  3619.  AccelaratorFunction
  3642.  WindowPlacementFlag
  3662.  OwnerDrawnType
  3686.  OwnerDrawnAction
  3712.  PeekMessageAction
  3731.  HotKeyModifier
  3752.  HotKeyIdentifier
  3772.  ExitWindowMode
  3794.  probvlem
  3802.  SetWindowPositionStyle
  3833.  probvlem
  3851.  KeyEvent
  3871.  MouseEvent
  3897.  QueueStatus
  3940.  SystemMetrics
  4003.  TrackPopMenu
  4026.  DrawTextMode
  4059.  DeviceContextFlag
  4090.  RedrawWindowFlag
  4120.  ScrollWindowFlag
  4141.  EnableScrollBarFlag
  4167.  MessageBoxValue
  4212.  ControlColor
  4238.  ColorType
  4277.  GetWindowFlag
  4302.  MenuFlag
  4352.  SystemCommand
  4388.  CursorIdentifier
  4420.  BitmapOption
  4472.  OCRFlag
  4502.  OICFlag
  4531.  IconIdentifier
  4554.  MessageBoxIdentifier
  4579.  EditStyle
  4610.  ControlNotification
  4636.  EditCtlBehaviour
  4683.  WordBreakFlag
  4702.  ButtonStyle
  4759.  ButtonMessageEnquiry
  4782.  StaticStyle
  4815.  StaticMessage
  4836.  DWLParameter
  4857.  DialogBoxFlag
  4884.  DialogStyle
  4909.  DefaultMessageAction
  4927.  PROB
  4935.  DialogCode
  4964.  ListBoxFlag
  5006.  ListBoxAction
  5063.  ListBoxStyle
  5099.  ComboBoxValue
  5149.  ComboBoxStyle
  5178.  ComboBoxAction
  5224.  ScrollBarStyle
  5251.  ScrollBarAction
  5285.  HelpOption
  5315.  SysParamInfo
  5368.  conversions
  5533.  upper case conversions
  5698.  ends
------------------------------------------------------------------------------*)


signature importMacroSig =
sig
   val hexOR : int list -> int
   val hex : string -> int
end;


(*------------------------------------------------------------------------------
INDEX: functor
------------------------------------------------------------------------------*)

functor ConstTypeFct ( structure Macro : importMacroSig 
                      ) =
struct

local
    val hex = Macro.hex
    val hexOR = Macro.hexOR


    fun MAKEINTRESOURCE i = i
    val TRUE = 1
    val FALSE = 0
    val DIFFERENCE = 11 
in

open CInterface

(*------------------------------------------------------------------------------
INDEX: misc
------------------------------------------------------------------------------*)



(*------------------------------------------------------------------------------
INDEX: 'a encoding wraps 'a <-> int
------------------------------------------------------------------------------*)


    abstype 'a encoding =
    E of (int -> 'a) * ('a -> int) 
    with
    val encoding = E
    fun absE (E (abs,rep)) = abs
    fun repE (E (abs,rep)) = rep
    end

    fun absConversion E C =
    let val (fromC,toC,ctype) = breakConversion C
    in  mkConversion (absE E o fromC) (toC o repE E) ctype
    end

    fun conversionOfEncoding E = absConversion E INT

    local
    (******
     * This scheme reduces the number of functions.
     * e.g. 40 types, t1 t2 t3 etc...
     *      with 40 t1 encoding values
     *      and the 4 general functions below.
     *      40 + 4 = 44 values.
     * This is equivalent to 40 * 4 = 160 values.
     ******)
    fun split n = (n mod 2=1,n div 2)
    fun extendBoolOp f (0,0) = 0
      | extendBoolOp f (n,m) = let val (n0,n) = split n
                       val (m0,m) = split m
                   in  (if f (n0,m0) then 1 else 0) + 2 * extendBoolOp f (n,m)
                   end
    fun extend bop E (a,b) = absE E (extendBoolOp bop (repE E a,repE E b))
    fun curry f a b = f (a,b)
    fun fold f Z [] = Z
      | fold f Z (x::xs) = fold f (f Z x) xs
    in
        val orE    = extend (fn (a,b) => a orelse b)
        val andE   = extend (fn (a,b) => a andalso b)
        val xorE   = extend (fn (a,b) => (not a andalso b) orelse (a andalso not b))
    fun orLE E = fold (curry (orE E)) (absE E 0)
    fun addE E = fn a => fn b => absE E (repE E a + repE E b)
    fun addLE E = fold (addE E) (absE E 0)      
    end



(*------------------------------------------------------------------------------
INDEX: DocumentMode
------------------------------------------------------------------------------*)

(* Device Modes *)
(* DocumentProperties() *)

(*TYPE: DocumentMode *)
local
    datatype DocumentMode =
    W of int
in
    type DocumentMode = DocumentMode
    val DocumentModeE = encoding (W,fn W n => n)

    val DM_UPDATE                                    = W (1)
    val DM_COPY                                      = W (2)
    val DM_PROMPT                                    = W (4)
    val DM_MODIFY                                    = W (8)
    val DM_IN_BUFFER                                 = DM_MODIFY
    val DM_IN_PROMPT                                 = DM_PROMPT
    val DM_OUT_BUFFER                                = DM_COPY
    val DM_OUT_DEFAULT                               = DM_UPDATE
end



(*------------------------------------------------------------------------------
INDEX: DeviceCapability
------------------------------------------------------------------------------*)

(* Device capabilities *)
(* DeviceCapabilities() *)

(*TYPE: DeviceCapability *)
local
    datatype DeviceCapability =
    W of int
in
    type DeviceCapability = DeviceCapability
    val DeviceCapabilityE = encoding (W,fn W n => n)

    val DC_FIELDS                                    = W (1)
    val DC_PAPERS                                    = W (2)
    val DC_PAPERSIZE                                 = W (3)
    val DC_MINEXTENT                                 = W (4)
    val DC_MAXEXTENT                                 = W (5)
    val DC_BINS                                      = W (6)
    val DC_DUPLEX                                    = W (7)
    val DC_SIZE                                      = W (8)
    val DC_EXTRA                                     = W (9)
    val DC_VERSION                                   = W (10)
    val DC_DRIVER                                    = W (11)
    val DC_BINNAMES                                  = W (12)
    val DC_ENUMRESOLUTIONS                           = W (13)
    val DC_FILEDEPENDENCIES                          = W (14)
    val DC_TRUETYPE                                  = W (15)
    val DC_PAPERNAMES                                = W (16)
    val DC_ORIENTATION                               = W (17)
    val DC_COPIES                                    = W (18)
end



(*------------------------------------------------------------------------------
INDEX: RasterOperation
------------------------------------------------------------------------------*)

(* Binary raster operations *)
(* GetROP2()
   SetROP2() *)

(*TYPE: RasterOperation *)
local
    datatype RasterOperation =
    W of int
in
    type RasterOperation = RasterOperation
    val RasterOperationE = encoding (W,fn W n => n)

    val R2_BLACK                                     = W (1 (* 0 *))
    val R2_NOTMERGEPEN                               = W (2 (* DPon *))
    val R2_MASKNOTPEN                                = W (3 (* DPna *))
    val R2_NOTCOPYPEN                                = W (4 (* PN *))
    val R2_MASKPENNOT                                = W (5 (* PDna *))
    val R2_NOT                                       = W (6 (* Dn *))
    val R2_XORPEN                                    = W (7 (* DPx *))
    val R2_NOTMASKPEN                                = W (8 (* DPan *))
    val R2_MASKPEN                                   = W (9 (* DPa *))
    val R2_NOTXORPEN                                 = W (10 (* DPxn *))
    val R2_NOP                                       = W (11 (* D *))
    val R2_MERGENOTPEN                               = W (12 (* DPno *))
    val R2_COPYPEN                                   = W (13 (* P *))
    val R2_MERGEPENNOT                               = W (14 (* PDno *))
    val R2_MERGEPEN                                  = W (15 (* DPo *))
    val R2_WHITE                                     = W (16 (* 1 *))
    val R2_LAST                                      = W (16)
end



(*------------------------------------------------------------------------------
INDEX: RasterOpCode
------------------------------------------------------------------------------*)

(* BitBlt() 
   MaskBlt() 
   StretchBlt() *)

(*TYPE: RasterOpCode *)
local
    datatype RasterOpCode =
    W of int
in
    type RasterOpCode = RasterOpCode
    val RasterOpCodeE = encoding (W,fn W n => n)

    val SRCCOPY                                      = W (hex "0x00CC0020" (* dest = source *))
    val SRCPAINT                                     = W (hex "0x00EE0086" (* dest = source OR dest *))
    val SRCAND                                       = W (hex "0x008800C6" (* dest = source AND dest *))
    val SRCINVERT                                    = W (hex "0x00660046" (* dest = source XOR dest *))
    val SRCERASE                                     = W (hex "0x00440328" (* dest = source AND (NOT dest ) *))
    val NOTSRCCOPY                                   = W (hex "0x00330008" (* dest = (NOT source) *))
    val NOTSRCERASE                                  = W (hex "0x001100A6" (* dest = (NOT src) AND (NOT dest) *))
    val MERGECOPY                                    = W (hex "0x00C000CA" (* dest = (source AND pattern) *))
    val MERGEPAINT                                   = W (hex "0x00BB0226" (* dest = (NOT source) OR dest *))
    val PATCOPY                                      = W (hex "0x00F00021" (* dest = pattern *))
    val PATPAINT                                     = W (hex "0x00FB0A09" (* dest = DPSnoo *))
    val PATINVERT                                    = W (hex "0x005A0049" (* dest = pattern XOR dest *))
    val DSTINVERT                                    = W (hex "0x00550009" (* dest = (NOT dest) *))
    val BLACKNESS                                    = W (hex "0x00000042" (* dest = BLACK *))
    val WHITENESS                                    = W (hex "0x00FF0062" (* dest = WHITE *))
end



(*------------------------------------------------------------------------------
INDEX: GdiError
------------------------------------------------------------------------------*)

(*TYPE: GdiError *)
local
    datatype GdiError =
    W of int
in
    type GdiError = GdiError
    val GdiErrorE = encoding (W,fn W n => n)

    val GDI_ERROR                                    = W ((hex "0xFFFFFFFF"))
    val HGDI_ERROR                                   = W ((hex "0xFFFFFFFF"))
end



(*------------------------------------------------------------------------------
INDEX: ResultRegion
------------------------------------------------------------------------------*)

(* CombineRgn()
   ExcludeClipRect()
   ExtSelectClipRgn()
   GetClipBox()
   GetRgnBox()
   IntersectClipRect() 
   OffsetClipRgn()
   OffsetRgn()
   SelectClipRgn()
   SelectObject()
   SetMetaRgn()
   ExcludeUpdateRgn()
   GetUpdateRgn()
   ScrollWindowEx()
 *)

(*TYPE: ResultRegion *)
local
    datatype ResultRegion =
    W of int
in
    type ResultRegion = ResultRegion
    val ResultRegionE = encoding (W,fn W n => n)

    val ERROR                                        = W (0)
    val NULLREGION                                   = W (1)
    val SIMPLEREGION                                 = W (2)
    val COMPLEXREGION                                = W (3)
end



(*------------------------------------------------------------------------------
INDEX: RegionOperation
------------------------------------------------------------------------------*)

(* Creates the union of the two combined regions except for the 
   overlapping areas.
   CombineRgn()
   ExtSelectClipRgn() 
   SelectClipPath()  *) 

(*TYPE: RegionOperation *)
local
    datatype RegionOperation =
    W of int
in
    type RegionOperation = RegionOperation
    val RegionOperationE = encoding (W,fn W n => n)

    val RGN_ERROR                                    = W (0)
    val RGN_AND                                      = W (1)
    val RGN_OR                                       = W (2)
    val RGN_XOR                                      = W (3)
    val RGN_DIFF                                     = W (4)
    val RGN_COPY                                     = W (5)
    val RGN_MIN                                      = RGN_AND
    val RGN_MAX                                      = RGN_COPY
end



(*------------------------------------------------------------------------------
INDEX: StretchMode
------------------------------------------------------------------------------*)

(* SetStretchBltMode() *)

(*TYPE: StretchMode *)
local
    datatype StretchMode =
    W of int
in
    type StretchMode = StretchMode
    val StretchModeE = encoding (W,fn W n => n)

    val BLACKONWHITE                                 = W (1)
    val WHITEONBLACK                                 = W (2)
    val COLORONCOLOR                                 = W (3)
    val HALFTONE                                     = W (4)
    val MAXSTRETCHBLTMODE                            = W (4)
end



(*------------------------------------------------------------------------------
INDEX: PolyFillMode
------------------------------------------------------------------------------*)

(* SetPolyFillMode()
   CreatePolygonRgn()
   CreatePolyPolygonRgn()
   GetPolyFillMode()
   GetDeviceCaps() *)

(*TYPE: PolyFillMode *)
local
    datatype PolyFillMode =
    W of int
in
    type PolyFillMode = PolyFillMode
    val PolyFillModeE = encoding (W,fn W n => n)

    val ALTERNATE                                    = W (1)
    val WINDING                                      = W (2)
    val POLYFILL_LAST                                = W (2)
end



(*------------------------------------------------------------------------------
INDEX: TextAlign
------------------------------------------------------------------------------*)

(* GetTextAlign() 
   SetTextAlign() *)

(*TYPE: TextAlign *)
local
    datatype TextAlign =
    W of int
in
    type TextAlign = TextAlign
    val TextAlignE = encoding (W,fn W n => n)

    val TA_NOUPDATECP                                = W (0)
    val TA_UPDATECP                                  = W (1)
    val TA_LEFT                                      = W (0)
    val TA_RIGHT                                     = W (2)
    val TA_CENTER                                    = W (6)
    val TA_TOP                                       = W (0)
    val TA_BOTTOM                                    = W (8)
    val TA_BASELINE                                  = W (24)
    val TA_MASK                                      = addLE TextAlignE [TA_BASELINE,TA_CENTER,TA_UPDATECP]
end



(*------------------------------------------------------------------------------
INDEX: VirtualTextAlign
------------------------------------------------------------------------------*)

(*TYPE: VirtualTextAlign *)
local
    datatype VirtualTextAlign =
    W of int
    fun f ta = W (repE TextAlignE ta)
in
    type VirtualTextAlign = VirtualTextAlign
    val VirtualTextAlignE = encoding (W,fn W n => n)

    val VTA_BASELINE                                 = f (TA_BASELINE)
    val VTA_LEFT                                     = f (TA_BOTTOM)
    val VTA_RIGHT                                    = f (TA_TOP)
    val VTA_CENTER                                   = f (TA_CENTER)
    val VTA_BOTTOM                                   = f (TA_RIGHT)
    val VTA_TOP                                      = f (TA_LEFT)
end



(*------------------------------------------------------------------------------
INDEX: ExtendedTextOut
------------------------------------------------------------------------------*)

(* PolyTextOut()
   ExtTextOut() 
   *)

(*TYPE: ExtendedTextOut *)
local
    datatype ExtendedTextOut =
    W of int
in
    type ExtendedTextOut = ExtendedTextOut
    val ExtendedTextOutE = encoding (W,fn W n => n)

    val ETO_GRAYED                                   = W (1)
    val ETO_OPAQUE                                   = W (2)
    val ETO_CLIPPED                                  = W (4)
end



(*------------------------------------------------------------------------------
INDEX: ASPECT_FILTERING constant
------------------------------------------------------------------------------*)

val ASPECT_FILTERING = 1


(*------------------------------------------------------------------------------
INDEX: DCBound
------------------------------------------------------------------------------*)

(* Drawing bounds *)
(* SetBoundsRect()
   GetBoundsRect()
   *)

(*TYPE: DCBound *)
local
    datatype DCBound =
    W of int
in
    type DCBound = DCBound
    val DCBoundE = encoding (W,fn W n => n)

    val DCB_RESET                                    = W (hex "0x0001")
    val DCB_ACCUMULATE                               = W (hex "0x0002")
    val DCB_DIRTY                                    = DCB_ACCUMULATE
    val DCB_SET                                      = orLE DCBoundE [DCB_RESET,DCB_ACCUMULATE]
    val DCB_ENABLE                                   = W (hex "0x0004")
    val DCB_DISABLE                                  = W (hex "0x0008")
end



(*------------------------------------------------------------------------------
INDEX: MetaFunction
------------------------------------------------------------------------------*)

(* Metafile functions *)

(*TYPE: MetaFunction *)
local
    datatype MetaFunction =
    W of int
in
    type MetaFunction = MetaFunction
    val MetaFunctionE = encoding (W,fn W n => n)

    val META_SETBKCOLOR                              = W (hex "0x0201")
    val META_SETBKMODE                               = W (hex "0x0102")
    val META_SETMAPMODE                              = W (hex "0x0103")
    val META_SETROP2                                 = W (hex "0x0104")
    val META_SETRELABS                               = W (hex "0x0105")
    val META_SETPOLYFILLMODE                         = W (hex "0x0106")
    val META_SETSTRETCHBLTMODE                       = W (hex "0x0107")
    val META_SETTEXTCHAREXTRA                        = W (hex "0x0108")
    val META_SETTEXTCOLOR                            = W (hex "0x0209")
    val META_SETTEXTJUSTIFICATION                    = W (hex "0x020A")
    val META_SETWINDOWORG                            = W (hex "0x020B")
    val META_SETWINDOWEXT                            = W (hex "0x020C")
    val META_SETVIEWPORTORG                          = W (hex "0x020D")
    val META_SETVIEWPORTEXT                          = W (hex "0x020E")
    val META_OFFSETWINDOWORG                         = W (hex "0x020F")
    val META_SCALEWINDOWEXT                          = W (hex "0x0410")
    val META_OFFSETVIEWPORTORG                       = W (hex "0x0211")
    val META_SCALEVIEWPORTEXT                        = W (hex "0x0412")
    val META_LINETO                                  = W (hex "0x0213")
    val META_MOVETO                                  = W (hex "0x0214")
    val META_EXCLUDECLIPRECT                         = W (hex "0x0415")
    val META_INTERSECTCLIPRECT                       = W (hex "0x0416")
    val META_ARC                                     = W (hex "0x0817")
    val META_ELLIPSE                                 = W (hex "0x0418")
    val META_FLOODFILL                               = W (hex "0x0419")
    val META_PIE                                     = W (hex "0x081A")
    val META_RECTANGLE                               = W (hex "0x041B")
    val META_ROUNDRECT                               = W (hex "0x061C")
    val META_PATBLT                                  = W (hex "0x061D")
    val META_SAVEDC                                  = W (hex "0x001E")
    val META_SETPIXEL                                = W (hex "0x041F")
    val META_OFFSETCLIPRGN                           = W (hex "0x0220")
    val META_TEXTOUT                                 = W (hex "0x0521")
    val META_BITBLT                                  = W (hex "0x0922")
    val META_STRETCHBLT                              = W (hex "0x0B23")
    val META_POLYGON                                 = W (hex "0x0324")
    val META_POLYLINE                                = W (hex "0x0325")
    val META_ESCAPE                                  = W (hex "0x0626")
    val META_RESTOREDC                               = W (hex "0x0127")
    val META_FILLREGION                              = W (hex "0x0228")
    val META_FRAMEREGION                             = W (hex "0x0429")
    val META_INVERTREGION                            = W (hex "0x012A")
    val META_PAINTREGION                             = W (hex "0x012B")
    val META_SELECTCLIPREGION                        = W (hex "0x012C")
    val META_SELECTOBJECT                            = W (hex "0x012D")
    val META_SETTEXTALIGN                            = W (hex "0x012E")
    val META_CHORD                                   = W (hex "0x0830")
    val META_SETMAPPERFLAGS                          = W (hex "0x0231")
    val META_EXTTEXTOUT                              = W (hex "0x0a32")
    val META_SETDIBTODEV                             = W (hex "0x0d33")
    val META_SELECTPALETTE                           = W (hex "0x0234")
    val META_REALIZEPALETTE                          = W (hex "0x0035")
    val META_ANIMATEPALETTE                          = W (hex "0x0436")
    val META_SETPALENTRIES                           = W (hex "0x0037")
    val META_POLYPOLYGON                             = W (hex "0x0538")
    val META_RESIZEPALETTE                           = W (hex "0x0139")
    val META_DIBBITBLT                               = W (hex "0x0940")
    val META_DIBSTRETCHBLT                           = W (hex "0x0b41")
    val META_DIBCREATEPATTERNBRUSH                   = W (hex "0x0142")
    val META_STRETCHDIB                              = W (hex "0x0f43")
    val META_EXTFLOODFILL                            = W (hex "0x0548")
    val META_DELETEOBJECT                            = W (hex "0x01f0")
    val META_CREATEPALETTE                           = W (hex "0x00f7")
    val META_CREATEPATTERNBRUSH                      = W (hex "0x01F9")
    val META_CREATEPENINDIRECT                       = W (hex "0x02FA")
    val META_CREATEFONTINDIRECT                      = W (hex "0x02FB")
    val META_CREATEBRUSHINDIRECT                     = W (hex "0x02FC")
    val META_CREATEREGION                            = W (hex "0x06FF")
end



(*------------------------------------------------------------------------------
INDEX: GDIEscapes
------------------------------------------------------------------------------*)

(*TYPE: GDIEscapes *)
local
    datatype GDIEscapes =
    W of int
in
    type GDIEscapes = GDIEscapes
    val GDIEscapesE = encoding (W,fn W n => n)

    val NEWFRAME                                     = W (1)
    val ABORTDOC                                     = W (2)
    val NEXTBAND                                     = W (3)
    val SETCOLORTABLE                                = W (4)
    val GETCOLORTABLE                                = W (5)
    val FLUSHOUTPUT                                  = W (6)
    val DRAFTMODE                                    = W (7)
    val QUERYESCSUPPORT                              = W (8)
    val SETABORTPROC                                 = W (9)
    val STARTDOC                                     = W (10)
    val ENDDOC                                       = W (11)
    val GETPHYSPAGESIZE                              = W (12)
    val GETPRINTINGOFFSET                            = W (13)
    val GETSCALINGFACTOR                             = W (14)
    val MFCOMMENT                                    = W (15)
    val GETPENWIDTH                                  = W (16)
    val SETCOPYCOUNT                                 = W (17)
    val SELECTPAPERSOURCE                            = W (18)
    val DEVICEDATA                                   = W (19)
    val PASSTHROUGH                                  = W (19)
    val GETTECHNOLGY                                 = W (20)
    val GETTECHNOLOGY                                = W (20)
    val SETLINECAP                                   = W (21)
    val SETLINEJOIN                                  = W (22)
    val SETMITERLIMIT                                = W (23)
    val BANDINFO                                     = W (24)
    val DRAWPATTERNRECT                              = W (25)
    val GETVECTORPENSIZE                             = W (26)
    val GETVECTORBRUSHSIZE                           = W (27)
    val ENABLEDUPLEX                                 = W (28)
    val GETSETPAPERBINS                              = W (29)
    val GETSETPRINTORIENT                            = W (30)
    val ENUMPAPERBINS                                = W (31)
    val SETDIBSCALING                                = W (32)
    val EPSPRINTING                                  = W (33)
    val ENUMPAPERMETRICS                             = W (34)
    val GETSETPAPERMETRICS                           = W (35)
    val POSTSCRIPT_DATA                              = W (37)
    val POSTSCRIPT_IGNORE                            = W (38)
    val MOUSETRAILS                                  = W (39)
    val GETDEVICEUNITS                               = W (42)
    val GETEXTENDEDTEXTMETRICS                       = W (256)
    val GETEXTENTTABLE                               = W (257)
    val GETPAIRKERNTABLE                             = W (258)
    val GETTRACKKERNTABLE                            = W (259)
    val EXTTEXTOUT                                   = W (512)
    val GETFACENAME                                  = W (513)
    val DOWNLOADFACE                                 = W (514)
    val ENABLERELATIVEWIDTHS                         = W (768)
    val ENABLEPAIRKERNING                            = W (769)
    val SETKERNTRACK                                 = W (770)
    val SETALLJUSTVALUES                             = W (771)
    val SETCHARSET                                   = W (772)
    val STRETCHBLT                                   = W (2048)
    val GETSETSCREENPARAMS                           = W (3072)
    val BEGIN_PATH                                   = W (4096)
    val CLIP_TO_PATH                                 = W (4097)
    val END_PATH                                     = W (4098)
    val EXT_DEVICE_CAPS                              = W (4099)
    val RESTORE_CTM                                  = W (4100)
    val SAVE_CTM                                     = W (4101)
    val SET_ARC_DIRECTION                            = W (4102)
    val SET_BACKGROUND_COLOR                         = W (4103)
    val SET_POLY_MODE                                = W (4104)
    val SET_SCREEN_ANGLE                             = W (4105)
    val SET_SPREAD                                   = W (4106)
    val TRANSFORM_CTM                                = W (4107)
    val SET_CLIP_BOX                                 = W (4108)
    val SET_BOUNDS                                   = W (4109)
    val SET_MIRROR_MODE                              = W (4110)
    val OPENCHANNEL                                  = W (4110)
    val DOWNLOADHEADER                               = W (4111)
    val CLOSECHANNEL                                 = W (4112)
    val POSTSCRIPT_PASSTHROUGH                       = W (4115)
    val ENCAPSULATED_POSTSCRIPT                      = W (4116)
end



(*------------------------------------------------------------------------------
INDEX: SpoolerErrorCode
------------------------------------------------------------------------------*)

(* AbortDoc()
   EndDoc()
   EndPage()
   Escape()
   StartDoc()
   StartPage() *)

(*TYPE: SpoolerErrorCode *)
local
    datatype SpoolerErrorCode =
    W of int
in
    type SpoolerErrorCode = SpoolerErrorCode
    val SpoolerErrorCodeE = encoding (W,fn W n => n)

    val SP_NOTREPORTED                               = W (hex "0x4000")
    val SP_ERROR                                     = W ((~1))
    val SP_APPABORT                                  = W ((~2))
    val SP_USERABORT                                 = W ((~3))
    val SP_OUTOFDISK                                 = W ((~4))
    val SP_OUTOFMEMORY                               = W ((~5))
    val PR_JOBSTATUS                                 = W (hex "0x0000")
end



(*------------------------------------------------------------------------------
INDEX: EnumObject
------------------------------------------------------------------------------*)

(* GetCurrentObject()
   GetObjectType()  *)

(*TYPE: EnumObject *)
local
    datatype EnumObject =
    W of int
in
    type EnumObject = EnumObject
    val EnumObjectE = encoding (W,fn W n => n)

    val OBJ_PEN                                      = W (1)
    val OBJ_BRUSH                                    = W (2)
    val OBJ_DC                                       = W (3)
    val OBJ_METADC                                   = W (4)
    val OBJ_PAL                                      = W (5)
    val OBJ_FONT                                     = W (6)
    val OBJ_BITMAP                                   = W (7)
    val OBJ_REGION                                   = W (8)
    val OBJ_METAFILE                                 = W (9)
    val OBJ_MEMDC                                    = W (10)
    val OBJ_EXTPEN                                   = W (11)
    val OBJ_ENHMETADC                                = W (12)
    val OBJ_ENHMETAFILE                              = W (13)
end



(*------------------------------------------------------------------------------
INDEX: XFormType
------------------------------------------------------------------------------*)

(* ModifyWorldTransform() *)

(*TYPE: XFormType *)
local
    datatype XFormType =
    W of int
in
    type XFormType = XFormType
    val XFormTypeE = encoding (W,fn W n => n)

    val MWT_IDENTITY                                 = W (1)
    val MWT_LEFTMULTIPLY                             = W (2)
    val MWT_RIGHTMULTIPLY                            = W (3)
    val MWT_MIN                                      = MWT_IDENTITY
    val MWT_MAX                                      = MWT_RIGHTMULTIPLY
end



(*------------------------------------------------------------------------------
INDEX: BitmapCompression
--------------------- ---------------------------------------------------------*)

(* Bitmap compression *)
(* BITMAPINFOHEADER struct *)

(*TYPE: BitmapCompression *)
local
    datatype BitmapCompression =
    W of int
in
    type BitmapCompression = BitmapCompression
    val BitmapCompressionE = encoding (W,fn W n => n)

    val BI_RGB                                       = W (0)
    val BI_RLE8                                      = W (1)
    val BI_RLE4                                      = W (2)
    val BI_BITFIELDS                                 = W (3)
end



(*------------------------------------------------------------------------------
INDEX: TextMetricPitchandFamily
------------------------------------------------------------------------------*)

(* TEXTMETRIC struct *)

(*TYPE: TextMetricPitchandFamily *)
local
    datatype TextMetricPitchandFamily =
    W of int
in
    type TextMetricPitchandFamily = TextMetricPitchandFamily
    val TextMetricPitchandFamilyE = encoding (W,fn W n => n)

    val TMPF_FIXED_PITCH                             = W (hex "0x01")
    val TMPF_VECTOR                                  = W (hex "0x02")
    val TMPF_DEVICE                                  = W (hex "0x08")
    val TMPF_TRUETYPE                                = W (hex "0x04")
end



(*------------------------------------------------------------------------------
INDEX: NewTextMetric
------------------------------------------------------------------------------*)

(*TYPE: NewTextMetric *)
local
    datatype NewTextMetric =
    W of int
in
    type NewTextMetric = NewTextMetric
    val NewTextMetricE = encoding (W,fn W n => n)

    val NTM_REGULAR                                  = W (hex "0x00000040")
    val NTM_BOLD                                     = W (hex "0x00000020")
    val NTM_ITALIC                                   = W (hex "0x00000001")
end    



(*------------------------------------------------------------------------------
INDEX: LogicalFont
------------------------------------------------------------------------------*)

(*TYPE: LogicalFont *)
local
    datatype LogicalFont =
    W of int
in
    type LogicalFont = LogicalFont
    val LogicalFontE = encoding (W,fn W n => n)

    val LF_FACESIZE                                  = W (32)
    val LF_FULLFACESIZE                              = W (64)
end



(*------------------------------------------------------------------------------
INDEX: OutputPrecision
------------------------------------------------------------------------------*)

(* CreateFont() *)

(*TYPE: OutputPrecision *)
local
    datatype OutputPrecision =
    W of int
in
    type OutputPrecision = OutputPrecision
    val OutputPrecisionE = encoding (W,fn W n => n)

    val OUT_DEFAULT_PRECIS                           = W (0)
    val OUT_STRING_PRECIS                            = W (1)
    val OUT_CHARACTER_PRECIS                         = W (2)
    val OUT_STROKE_PRECIS                            = W (3)
    val OUT_TT_PRECIS                                = W (4)
    val OUT_DEVICE_PRECIS                            = W (5)
    val OUT_RASTER_PRECIS                            = W (6)
    val OUT_TT_ONLY_PRECIS                           = W (7)
    val OUT_OUTLINE_PRECIS                           = W (8)
end



(*------------------------------------------------------------------------------
INDEX: ClippingPrecision
------------------------------------------------------------------------------*)

(* Logical Font 
   CreateFont() *)

(*TYPE: ClippingPrecision *)
local
    datatype ClippingPrecision =
    W of int
in
    type ClippingPrecision = ClippingPrecision
    val ClippingPrecisionE = encoding (W,fn W n => n)

    val CLIP_DEFAULT_PRECIS                          = W (0)
    val CLIP_CHARACTER_PRECIS                        = W (1)
    val CLIP_STROKE_PRECIS                           = W (2)
    val CLIP_MASK                                    = W (hex "0xf")
    val CLIP_LH_ANGLES                               = W (1 * 16 (* 1<<4 *))
    val CLIP_TT_ALWAYS                               = W (2 * 16 (* 2<<4 *))
    val CLIP_EMBEDDED                                = W (8 * 16 (* 8<<4 *))
end



(*------------------------------------------------------------------------------
INDEX: OutputQuality
------------------------------------------------------------------------------*)

(* CreateFont() *)

(*TYPE: OutputQuality *)
local
    datatype OutputQuality =
    W of int
in
    type OutputQuality = OutputQuality
    val OutputQualityE = encoding (W,fn W n => n)

    val DEFAULT_QUALITY                              = W (0)
    val DRAFT_QUALITY                                = W (1)
    val PROOF_QUALITY                                = W (2)
end



(*------------------------------------------------------------------------------
INDEX: FontPitch
------------------------------------------------------------------------------*)

(* CreateFont() *)

(*TYPE: FontPitch *)
local
    datatype FontPitch =
    W of int
in
    type FontPitch = FontPitch
    val FontPitchE = encoding (W,fn W n => n)

    val DEFAULT_PITCH                                = W (0)
    val FIXED_PITCH                                  = W (1)
    val VARIABLE_PITCH                               = W (2)
end



(*------------------------------------------------------------------------------
INDEX: CharacterSet
------------------------------------------------------------------------------*)

(* CreateFont() *)

(*TYPE: CharacterSet *)
local
    datatype CharacterSet =
    W of int
in
    type CharacterSet = CharacterSet
    val CharacterSetE = encoding (W,fn W n => n)

    val ANSI_CHARSET                                 = W (0)
    val DEFAULT_CHARSET                              = W (1)
    val SYMBOL_CHARSET                               = W (2)
    val SHIFTJIS_CHARSET                             = W (128)
    val HANGEUL_CHARSET                              = W (129)
    val CHINESEBIG5_CHARSET                          = W (136)
    val OEM_CHARSET                                  = W (255)
end



(*------------------------------------------------------------------------------
INDEX: FontFamily
------------------------------------------------------------------------------*)

(* CreateFont() *)

(*TYPE: FontFamily *)
local
    datatype FontFamily =
    W of int
in
    type FontFamily = FontFamily
    val FontFamilyE = encoding (W,fn W n => n)

    val FF_DONTCARE                                  = W (0 * 16 (* (0<<4) Don't care or don't know. *))
    val FF_ROMAN                                     = W (1 * 16 (* (1<<4) Variable stroke width, serifed. *))
    val FF_SWISS                                     = W (2 * 16 (* (2<<4) Variable stroke width, sans~serifed. *))
    val FF_MODERN                                    = W (3 * 16 (* (3<<4) Constant stroke width, serifed or sans~serifed. *))
    val FF_SCRIPT                                    = W (4 * 16 (* (4<<4) Cursive, etc. *))
    val FF_DECORATIVE                                = W (5 * 16 (* (5<<4) Old English, etc. *))
end



(*------------------------------------------------------------------------------
INDEX: FontWeight
------------------------------------------------------------------------------*)

(* CreateFont() *)

(*TYPE: FontWeight *)
local
    datatype FontWeight =
    W of int
in
    type FontWeight = FontWeight
    val FontWeightE = encoding (W,fn W n => n)

    val FW_DONTCARE                                  = W (0)
    val FW_THIN                                      = W (100)
    val FW_EXTRALIGHT                                = W (200)
    val FW_LIGHT                                     = W (300)
    val FW_NORMAL                                    = W (400)
    val FW_MEDIUM                                    = W (500)
    val FW_SEMIBOLD                                  = W (600)
    val FW_BOLD                                      = W (700)
    val FW_EXTRABOLD                                 = W (800)
    val FW_HEAVY                                     = W (900)
    val FW_ULTRALIGHT                                = FW_EXTRALIGHT
    val FW_REGULAR                                   = FW_NORMAL
    val FW_DEMIBOLD                                  = FW_SEMIBOLD
    val FW_ULTRABOLD                                 = FW_EXTRABOLD
    val FW_BLACK                                     = FW_HEAVY
end



(*------------------------------------------------------------------------------
INDEX: CulturePANOSE
------------------------------------------------------------------------------*)

(* In PANOSE struct *)

(*TYPE: CulturePANOSE *)
local
    datatype CulturePANOSE =
    W of int
in
    type CulturePANOSE = CulturePANOSE
    val CulturePANOSEE = encoding (W,fn W n => n)

    val PAN_ANY                                      = W (0 (* Any *))
    val PAN_CULTURE_LATIN                            = W (0)
end



(*------------------------------------------------------------------------------
INDEX: FamilyTypePANOSE
------------------------------------------------------------------------------*)

(*TYPE: FamilyTypePANOSE *)
local
    datatype FamilyTypePANOSE =
    W of int
in
    type FamilyTypePANOSE = FamilyTypePANOSE
    val FamilyTypePANOSEE = encoding (W,fn W n => n)

    val PAN_ANY                                      = W (0 (* Any *))
    val PAN_NO_FIT                                   = W (1 (* No Fit *))
    val PAN_FAMILY_TEXT_DISPLAY                      = W (2 (* Text and Display *))
    val PAN_FAMILY_SCRIPT                            = W (3 (* Script *))
    val PAN_FAMILY_DECORATIVE                        = W (4 (* Decorative *))
    val PAN_FAMILY_PICTORIAL                         = W (5 (* Pictorial *))
end



(*------------------------------------------------------------------------------
INDEX: IndexPANOSE
------------------------------------------------------------------------------*)

(*TYPE: IndexPANOSE *)
local
    datatype IndexPANOSE =
    W of int
in
    type IndexPANOSE = IndexPANOSE
    val IndexPANOSEE = encoding (W,fn W n => n)

    val PANOSE_COUNT                                 = W (10)
    val PAN_FAMILYTYPE_INDEX                         = W (0)
    val PAN_SERIFSTYLE_INDEX                         = W (1)
    val PAN_WEIGHT_INDEX                             = W (2)
    val PAN_PROPORTION_INDEX                         = W (3)
    val PAN_CONTRAST_INDEX                           = W (4)
    val PAN_STROKEVARIATION_INDEX                    = W (5)
    val PAN_ARMSTYLE_INDEX                           = W (6)
    val PAN_LETTERFORM_INDEX                         = W (7)
    val PAN_MIDLINE_INDEX                            = W (8)
    val PAN_XHEIGHT_INDEX                            = W (9)
end



(*------------------------------------------------------------------------------
INDEX: SerifStylePANOSE
------------------------------------------------------------------------------*)

(*TYPE: SerifStylePANOSE *)
local
    datatype SerifStylePANOSE =
    W of int
in
    type SerifStylePANOSE = SerifStylePANOSE
    val SerifStylePANOSEE = encoding (W,fn W n => n)

    val PAN_ANY                                      = W (0 (* Any *))
    val PAN_NO_FIT                                   = W (1 (* No Fit *))
    val PAN_SERIF_COVE                               = W (2 (* Cove *))
    val PAN_SERIF_OBTUSE_COVE                        = W (3 (* Obtuse Cove *))
    val PAN_SERIF_SQUARE_COVE                        = W (4 (* Square Cove *))
    val PAN_SERIF_OBTUSE_SQUARE_COVE                 = W (5 (* Obtuse Square Cove *))
    val PAN_SERIF_SQUARE                             = W (6 (* Square *))
    val PAN_SERIF_THIN                               = W (7 (* Thin *))
    val PAN_SERIF_BONE                               = W (8 (* Bone *))
    val PAN_SERIF_EXAGGERATED                        = W (9 (* Exaggerated *))
    val PAN_SERIF_TRIANGLE                           = W (10 (* Triangle *))
    val PAN_SERIF_NORMAL_SANS                        = W (11 (* Normal Sans *))
    val PAN_SERIF_OBTUSE_SANS                        = W (12 (* Obtuse Sans *))
    val PAN_SERIF_PERP_SANS                          = W (13 (* Prep Sans *))
    val PAN_SERIF_FLARED                             = W (14 (* Flared *))
    val PAN_SERIF_ROUNDED                            = W (15 (* Rounded *))
end



(*------------------------------------------------------------------------------
INDEX: WeightPANOSE
------------------------------------------------------------------------------*)

(*TYPE: WeightPANOSE *)
local
    datatype WeightPANOSE =
    W of int
in
    type WeightPANOSE = WeightPANOSE
    val WeightPANOSEE = encoding (W,fn W n => n)

    val PAN_ANY                                      = W (0 (* Any *))
    val PAN_NO_FIT                                   = W (1 (* No Fit *))
    val PAN_WEIGHT_VERY_LIGHT                        = W (2 (* Very Light *))
    val PAN_WEIGHT_LIGHT                             = W (3 (* Light *))
    val PAN_WEIGHT_THIN                              = W (4 (* Thin *))
    val PAN_WEIGHT_BOOK                              = W (5 (* Book *))
    val PAN_WEIGHT_MEDIUM                            = W (6 (* Medium *))
    val PAN_WEIGHT_DEMI                              = W (7 (* Demi *))
    val PAN_WEIGHT_BOLD                              = W (8 (* Bold *))
    val PAN_WEIGHT_HEAVY                             = W (9 (* Heavy *))
    val PAN_WEIGHT_BLACK                             = W (10 (* Black *))
    val PAN_WEIGHT_NORD                              = W (11 (* Nord *))
end



(*------------------------------------------------------------------------------
INDEX: ProportionPANOSE
------------------------------------------------------------------------------*)

(*TYPE: ProportionPANOSE *)
local
    datatype ProportionPANOSE =
    W of int
in
    type ProportionPANOSE = ProportionPANOSE
    val ProportionPANOSEE = encoding (W,fn W n => n)

    val PAN_ANY                                      = W (0 (* Any *))
    val PAN_NO_FIT                                   = W (1 (* No Fit *))
    val PAN_PROP_OLD_STYLE                           = W (2 (* Old Style *))
    val PAN_PROP_MODERN                              = W (3 (* Modern *))
    val PAN_PROP_EVEN_WIDTH                          = W (4 (* Even Width *))
    val PAN_PROP_EXPANDED                            = W (5 (* Expanded *))
    val PAN_PROP_CONDENSED                           = W (6 (* Condensed *))
    val PAN_PROP_VERY_EXPANDED                       = W (7 (* Very Expanded *))
    val PAN_PROP_VERY_CONDENSED                      = W (8 (* Very Condensed *))
    val PAN_PROP_MONOSPACED                          = W (9 (* Monospaced *))
end



(*------------------------------------------------------------------------------
INDEX: ContrastPANOSE
------------------------------------------------------------------------------*)

(*TYPE: ContrastPANOSE *)
local
    datatype ContrastPANOSE =
    W of int
in
    type ContrastPANOSE = ContrastPANOSE
    val ContrastPANOSEE = encoding (W,fn W n => n)

    val PAN_ANY                                      = W (0 (* Any *))
    val PAN_NO_FIT                                   = W (1 (* No Fit *))
    val PAN_CONTRAST_NONE                            = W (2 (* None *))
    val PAN_CONTRAST_VERY_LOW                        = W (3 (* Very Low *))
    val PAN_CONTRAST_LOW                             = W (4 (* Low *))
    val PAN_CONTRAST_MEDIUM_LOW                      = W (5 (* Medium Low *))
    val PAN_CONTRAST_MEDIUM                          = W (6 (* Medium *))
    val PAN_CONTRAST_MEDIUM_HIGH                     = W (7 (* Mediim High *))
    val PAN_CONTRAST_HIGH                            = W (8 (* High *))
    val PAN_CONTRAST_VERY_HIGH                       = W (9 (* Very High *))
end



(*------------------------------------------------------------------------------
INDEX: StrokeVariationPANOSE
------------------------------------------------------------------------------*)

(*TYPE: StrokeVariationPANOSE *)
local
    datatype StrokeVariationPANOSE =
    W of int
in
    type StrokeVariationPANOSE = StrokeVariationPANOSE
    val StrokeVariationPANOSEE = encoding (W,fn W n => n)

    val PAN_ANY                                      = W (0 (* Any *))
    val PAN_NO_FIT                                   = W (1 (* No Fit *))
    val PAN_STROKE_GRADUAL_DIAG                      = W (2 (* Gradual/Diagonal *))
    val PAN_STROKE_GRADUAL_TRAN                      = W (3 (* Gradual/Transitional *))
    val PAN_STROKE_GRADUAL_VERT                      = W (4 (* Gradual/Vertical *))
    val PAN_STROKE_GRADUAL_HORZ                      = W (5 (* Gradual/Horizontal *))
    val PAN_STROKE_RAPID_VERT                        = W (6 (* Rapid/Vertical *))
    val PAN_STROKE_RAPID_HORZ                        = W (7 (* Rapid/Horizontal *))
    val PAN_STROKE_INSTANT_VERT                      = W (8 (* Instant/Vertical *))
end



(*------------------------------------------------------------------------------
INDEX: ArmStylePANOSE
------------------------------------------------------------------------------*)

(*TYPE: ArmStylePANOSE *)
local
    datatype ArmStylePANOSE =
    W of int
in
    type ArmStylePANOSE = ArmStylePANOSE
    val ArmStylePANOSEE = encoding (W,fn W n => n)

    val PAN_ANY                                      = W (0 (* Any *))
    val PAN_NO_FIT                                   = W (1 (* No Fit *))
    val PAN_STRAIGHT_ARMS_HORZ                       = W (2 (* Straight Arms/Horizontal *))
    val PAN_STRAIGHT_ARMS_WEDGE                      = W (3 (* Straight Arms/Wedge *))
    val PAN_STRAIGHT_ARMS_VERT                       = W (4 (* Straight Arms/Vertical *))
    val PAN_STRAIGHT_ARMS_SINGLE_SERIF               = W (5 (* Straight Arms/Single~Serif *))
    val PAN_STRAIGHT_ARMS_DOUBLE_SERIF               = W (6 (* Straight Arms/Double~Serif *))
    val PAN_BENT_ARMS_HORZ                           = W (7 (* Non~Straight Arms/Horizontal *))
    val PAN_BENT_ARMS_WEDGE                          = W (8 (* Non~Straight Arms/Wedge *))
    val PAN_BENT_ARMS_VERT                           = W (9 (* Non~Straight Arms/Vertical *))
    val PAN_BENT_ARMS_SINGLE_SERIF                   = W (10 (* Non~Straight Arms/Single~Serif *))
    val PAN_BENT_ARMS_DOUBLE_SERIF                   = W (11 (* Non~Straight Arms/Double~Serif *))
end



(*------------------------------------------------------------------------------
INDEX: LetterFormPANOSE
------------------------------------------------------------------------------*)

(*TYPE: LetterFormPANOSE *)
local
    datatype LetterFormPANOSE =
    W of int
in
    type LetterFormPANOSE = LetterFormPANOSE
    val LetterFormPANOSEE = encoding (W,fn W n => n)

    val PAN_ANY                                      = W (0 (* Any *))
    val PAN_NO_FIT                                   = W (1 (* No Fit *))
    val PAN_LETT_NORMAL_CONTACT                      = W (2 (* Normal/Contact *))
    val PAN_LETT_NORMAL_WEIGHTED                     = W (3 (* Normal/Weighted *))
    val PAN_LETT_NORMAL_BOXED                        = W (4 (* Normal/Boxed *))
    val PAN_LETT_NORMAL_FLATTENED                    = W (5 (* Normal/Flattened *))
    val PAN_LETT_NORMAL_ROUNDED                      = W (6 (* Normal/Rounded *))
    val PAN_LETT_NORMAL_OFF_CENTER                   = W (7 (* Normal/Off Center *))
    val PAN_LETT_NORMAL_SQUARE                       = W (8 (* Normal/Square *))
    val PAN_LETT_OBLIQUE_CONTACT                     = W (9 (* Oblique/Contact *))
    val PAN_LETT_OBLIQUE_WEIGHTED                    = W (10 (* Oblique/Weighted *))
    val PAN_LETT_OBLIQUE_BOXED                       = W (11 (* Oblique/Boxed *))
    val PAN_LETT_OBLIQUE_FLATTENED                   = W (12 (* Oblique/Flattened *))
    val PAN_LETT_OBLIQUE_ROUNDED                     = W (13 (* Oblique/Rounded *))
    val PAN_LETT_OBLIQUE_OFF_CENTER                  = W (14 (* Oblique/Off Center *))
    val PAN_LETT_OBLIQUE_SQUARE                      = W (15 (* Oblique/Square *))
end



(*------------------------------------------------------------------------------
INDEX: MidlinePANOSE
------------------------------------------------------------------------------*)

(*TYPE: MidlinePANOSE *)
local
    datatype MidlinePANOSE =
    W of int
in
    type MidlinePANOSE = MidlinePANOSE
    val MidlinePANOSEE = encoding (W,fn W n => n)

    val PAN_ANY                                      = W (0 (* Any *))
    val PAN_NO_FIT                                   = W (1 (* No Fit *))
    val PAN_MIDLINE_STANDARD_TRIMMED                 = W (2 (* Standard/Trimmed *))
    val PAN_MIDLINE_STANDARD_POINTED                 = W (3 (* Standard/Pointed *))
    val PAN_MIDLINE_STANDARD_SERIFED                 = W (4 (* Standard/Serifed *))
    val PAN_MIDLINE_HIGH_TRIMMED                     = W (5 (* High/Trimmed *))
    val PAN_MIDLINE_HIGH_POINTED                     = W (6 (* High/Pointed *))
    val PAN_MIDLINE_HIGH_SERIFED                     = W (7 (* High/Serifed *))
    val PAN_MIDLINE_CONSTANT_TRIMMED                 = W (8 (* Constant/Trimmed *))
    val PAN_MIDLINE_CONSTANT_POINTED                 = W (9 (* Constant/Pointed *))
    val PAN_MIDLINE_CONSTANT_SERIFED                 = W (10 (* Constant/Serifed *))
    val PAN_MIDLINE_LOW_TRIMMED                      = W (11 (* Low/Trimmed *))
    val PAN_MIDLINE_LOW_POINTED                      = W (12 (* Low/Pointed *))
    val PAN_MIDLINE_LOW_SERIFED                      = W (13 (* Low/Serifed *))
end



(*------------------------------------------------------------------------------
INDEX: XHeightPANOSE
------------------------------------------------------------------------------*)

(*TYPE: XHeightPANOSE *)
local
    datatype XHeightPANOSE =
    W of int
in
    type XHeightPANOSE = XHeightPANOSE
    val XHeightPANOSEE = encoding (W,fn W n => n)

    val PAN_ANY                                      = W (0 (* Any *))
    val PAN_NO_FIT                                   = W (1 (* No Fit *))
    val PAN_XHEIGHT_CONSTANT_SMALL                   = W (2 (* Constant/Small *))
    val PAN_XHEIGHT_CONSTANT_STD                     = W (3 (* Constant/Standard *))
    val PAN_XHEIGHT_CONSTANT_LARGE                   = W (4 (* Constant/Large *))
    val PAN_XHEIGHT_DUCKING_SMALL                    = W (5 (* Ducking/Small *))
    val PAN_XHEIGHT_DUCKING_STD                      = W (6 (* Ducking/Standard *))
    val PAN_XHEIGHT_DUCKING_LARGE                    = W (7 (* Ducking/Large *))
end



(*------------------------------------------------------------------------------
INDEX: ExtLogFont
------------------------------------------------------------------------------*)

(*TYPE: ExtLogFont *)
local
    datatype ExtLogFont =
    W of int
in
    type ExtLogFont = ExtLogFont
    val ExtLogFontE = encoding (W,fn W n => n)

    val ELF_VENDOR_SIZE                              = W (4)
    val ELF_VERSION                                  = W (0)
    val ELF_CULTURE_LATIN                            = W (0)
end



(*------------------------------------------------------------------------------
INDEX: FontType
------------------------------------------------------------------------------*)

(*TYPE: FontType *)
local
    datatype FontType =
    W of int
in
    type FontType = FontType
    val FontTypeE = encoding (W,fn W n => n)

    val RASTER_FONTTYPE                              = W (hex "0x0001")
    val DEVICE_FONTTYPE                              = W (hex "0x002")
    val TRUETYPE_FONTTYPE                            = W (hex "0x004")
end



(*------------------------------------------------------------------------------
INDEX: PaletteEntryFlag
------------------------------------------------------------------------------*)

(* AnimatePalette() *)

(*TYPE: PaletteEntryFlag *)
local
    datatype PaletteEntryFlag =
    W of int
in
    type PaletteEntryFlag = PaletteEntryFlag
    val PaletteEntryFlagE = encoding (W,fn W n => n)

    val PC_RESERVED                                  = W (hex "0x01" (* palette index used for animation *))
    val PC_EXPLICIT                                  = W (hex "0x02" (* palette index is explicit to device *))
    val PC_NOCOLLAPSE                                = W (hex "0x04" (* do not match color to system palette *))
end



(*------------------------------------------------------------------------------
INDEX: BkMode
------------------------------------------------------------------------------*)

(* TextOut()
   SetBkMode() *)

(*TYPE: BkMode *)
local
    datatype BkMode =
    W of int
in
    type BkMode = BkMode
    val BkModeE = encoding (W,fn W n => n)

    val TRANSPARENT                                  = W (1)
    val OPAQUE                                       = W (2)
    val BKMODE_LAST                                  = W (2)
end



(*------------------------------------------------------------------------------
INDEX: GraphicsMode
------------------------------------------------------------------------------*)

(* SetGraphicsMode()
   GetGraphicsMode()  *)

(*TYPE: GraphicsMode *)
local
    datatype GraphicsMode =
    W of int
in
    type GraphicsMode = GraphicsMode
    val GraphicsModeE = encoding (W,fn W n => n)

    val GM_COMPATIBLE                                = W (1)
    val GM_ADVANCED                                  = W (2)
    val GM_LAST                                      = W (2)
end



(*------------------------------------------------------------------------------
INDEX: PointType
------------------------------------------------------------------------------*)

(* PolyDraw()
   GetPath()   *)

(*TYPE: PointType *)
local
    datatype PointType =
    W of int
in
    type PointType = PointType
    val PointTypeE = encoding (W,fn W n => n)

    val PT_CLOSEFIGURE                               = W (hex "0x01")
    val PT_LINETO                                    = W (hex "0x02")
    val PT_BEZIERTO                                  = W (hex "0x04")
    val PT_MOVETO                                    = W (hex "0x06")
end



(*------------------------------------------------------------------------------
INDEX: MapMode
------------------------------------------------------------------------------*)

(* SetMapMode() 
   GetMapMode()  *) 

(*TYPE: MapMode *)
local
    datatype MapMode =
    W of int
in
    type MapMode = MapMode
    val MapModeE = encoding (W,fn W n => n)

    val MM_TEXT                                      = W (1)
    val MM_LOMETRIC                                  = W (2)
    val MM_HIMETRIC                                  = W (3)
    val MM_LOENGLISH                                 = W (4)
    val MM_HIENGLISH                                 = W (5)
    val MM_TWIPS                                     = W (6)
    val MM_ISOTROPIC                                 = W (7)
    val MM_ANISOTROPIC                               = W (8)
    val MM_MIN                                       = MM_TEXT
    val MM_MAX                                       = MM_ANISOTROPIC
    val MM_MAX_FIXEDSCALE                            = MM_TWIPS
end



(*------------------------------------------------------------------------------
INDEX: TransMode
------------------------------------------------------------------------------*)

(*TYPE: TransMode *)
local
    datatype TransMode =
    W of int
in
    type TransMode = TransMode
    val TransModeE = encoding (W,fn W n => n)

    val ABSOLUTE                                     = W (1)
    val RELATIVE                                     = W (2)
end



(*------------------------------------------------------------------------------
INDEX: StockObjectType
------------------------------------------------------------------------------*)

(* GetStockObject() *)

(*TYPE: StockObjectType *)
local
    datatype StockObjectType =
    W of int
in
    type StockObjectType = StockObjectType
    val StockObjectTypeE = encoding (W,fn W n => n)

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
    val CLR_INVALID                                  = W (hex "0xFFFFFFFF")
end



(*------------------------------------------------------------------------------
INDEX: BrushStyle
------------------------------------------------------------------------------*)

(*TYPE: BrushStyle *)
local
    datatype BrushStyle =
    W of int
in
    type BrushStyle = BrushStyle
    val BrushStyleE = encoding (W,fn W n => n)

    val BS_SOLID                                     = W (0)
    val BS_NULL                                      = W (1)
    val BS_HOLLOW                                    = BS_NULL
    val BS_HATCHED                                   = W (2)
    val BS_PATTERN                                   = W (3)
    val BS_INDEXED                                   = W (4)
    val BS_DIBPATTERN                                = W (5)
    val BS_DIBPATTERNPT                              = W (6)
    val BS_PATTERN8X8                                = W (7)
    val BS_DIBPATTERN8X8                             = W (8)
end



(*------------------------------------------------------------------------------
INDEX: HatchStyle
------------------------------------------------------------------------------*)

(*TYPE: HatchStyle *)
local
    datatype HatchStyle =
    W of int
in
    type HatchStyle = HatchStyle
    val HatchStyleE = encoding (W,fn W n => n)

    val HS_HORIZONTAL                                = W (0 (* ~~~~~ *))
    val HS_VERTICAL                                  = W (1 (* ||||| *))
    val HS_FDIAGONAL                                 = W (2 (* \\\\\ *))
    val HS_BDIAGONAL                                 = W (3 (* (* /// *) *))
    val HS_CROSS                                     = W (4 (* +++++ *))
    val HS_DIAGCROSS                                 = W (5 (* xxxxx *))
    val HS_FDIAGONAL1                                = W (6)
    val HS_BDIAGONAL1                                = W (7)
    val HS_SOLID                                     = W (8)
    val HS_DENSE1                                    = W (9)
    val HS_DENSE2                                    = W (10)
    val HS_DENSE3                                    = W (11)
    val HS_DENSE4                                    = W (12)
    val HS_DENSE5                                    = W (13)
    val HS_DENSE6                                    = W (14)
    val HS_DENSE7                                    = W (15)
    val HS_DENSE8                                    = W (16)
    val HS_NOSHADE                                   = W (17)
    val HS_HALFTONE                                  = W (18)
    val HS_SOLIDCLR                                  = W (19)
    val HS_DITHEREDCLR                               = W (20)
    val HS_SOLIDTEXTCLR                              = W (21)
    val HS_DITHEREDTEXTCLR                           = W (22)
    val HS_SOLIDBKCLR                                = W (23)
    val HS_DITHEREDBKCLR                             = W (24)
    val HS_API_MAX                                   = W (25)
end



(*------------------------------------------------------------------------------
INDEX: PenStyle
------------------------------------------------------------------------------*)

(* Pen Styles *)
(* ExtCreatePen()  *)

(*TYPE: PenStyle *)
local
    datatype PenStyle =
    W of int
in
    type PenStyle = PenStyle
    val PenStyleE = encoding (W,fn W n => n)

    val PS_SOLID                                     = W (0)
    val PS_DASH                                      = W (1 (* ~~~~~~~ *))
    val PS_DOT                                       = W (2 (* ....... *))
    val PS_DASHDOT                                   = W (3 (* _._._._ *))
    val PS_DASHDOTDOT                                = W (4 (* _.._.._ *))
    val PS_NULL                                      = W (5)
    val PS_INSIDEFRAME                               = W (6)
    val PS_USERSTYLE                                 = W (7)
    val PS_ALTERNATE                                 = W (8)
    val PS_STYLE_MASK                                = W (hex "0x0000000F")
    val PS_ENDCAP_ROUND                              = W (hex "0x00000000")
    val PS_ENDCAP_SQUARE                             = W (hex "0x00000100")
    val PS_ENDCAP_FLAT                               = W (hex "0x00000200")
    val PS_ENDCAP_MASK                               = W (hex "0x00000F00")
    val PS_JOIN_ROUND                                = W (hex "0x00000000")
    val PS_JOIN_BEVEL                                = W (hex "0x00001000")
    val PS_JOIN_MITER                                = W (hex "0x00002000")
    val PS_JOIN_MASK                                 = W (hex "0x0000F000")
    val PS_COSMETIC                                  = W (hex "0x00000000")
    val PS_GEOMETRIC                                 = W (hex "0x00010000")
    val PS_TYPE_MASK                                 = W (hex "0x000F0000")
end



(*------------------------------------------------------------------------------
INDEX: ArcDirection
------------------------------------------------------------------------------*)

local
    datatype ArcDirection =
    W of int
in
    type ArcDirection = ArcDirection
    val ArcDirectionE = encoding (W,fn W n => n)

    val AD_COUNTERCLOCKWISE                          = W(1)
    val AD_CLOCKWISE                                 = W(2)
end;


(*------------------------------------------------------------------------------
INDEX: DeviceItem
------------------------------------------------------------------------------*)

(* GetDeviceCaps()  *)

(*TYPE: DeviceItem *)
local
    datatype DeviceItem =
    W of int
in
    type DeviceItem = DeviceItem
    val DeviceItemE = encoding (W,fn W n => n)

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



(*------------------------------------------------------------------------------
INDEX: DeviceTechnology
------------------------------------------------------------------------------*)

(* GetDeviceCaps()  *)

(*TYPE: DeviceTechnology *)
local
    datatype DeviceTechnology =
    W of int
in
    type DeviceTechnology = DeviceTechnology
    val DeviceTechnologyE = encoding (W,fn W n => n)

    val DT_PLOTTER                                   = W (0 (* Vector plotter *))
    val DT_RASDISPLAY                                = W (1 (* Raster display *))
    val DT_RASPRINTER                                = W (2 (* Raster printer *))
    val DT_RASCAMERA                                 = W (3 (* Raster camera *))
    val DT_CHARSTREAM                                = W (4 (* Character~stream, PLP *))
    val DT_METAFILE                                  = W (5 (* Metafile, VDM *))
    val DT_DISPFILE                                  = W (6 (* Display~file *))
end



(*------------------------------------------------------------------------------
INDEX: CurveCapability
------------------------------------------------------------------------------*)

(* GetDeviceCaps()  *)

(*TYPE: CurveCapability *)
local
    datatype CurveCapability =
    W of int
in
    type CurveCapability = CurveCapability
    val CurveCapabilityE = encoding (W,fn W n => n)

    val CC_NONE                                      = W (0 (* Curves not supported *))
    val CC_CIRCLES                                   = W (1 (* Can do circles *))
    val CC_PIE                                       = W (2 (* Can do pie wedges *))
    val CC_CHORD                                     = W (4 (* Can do chord arcs *))
    val CC_ELLIPSES                                  = W (8 (* Can do ellipese *))
    val CC_WIDE                                      = W (16 (* Can do wide lines *))
    val CC_STYLED                                    = W (32 (* Can do styled lines *))
    val CC_WIDESTYLED                                = W (64 (* Can do wide styled lines *))
    val CC_INTERIORS                                 = W (128 (* Can do interiors *))
    val CC_ROUNDRECT                                 = W (256 (* *))
end



(*------------------------------------------------------------------------------
INDEX: LineCapability
------------------------------------------------------------------------------*)

(* Line capabilities *)
(* GetDeviceCaps()  *)

(*TYPE: LineCapability *)
local
    datatype LineCapability =
    W of int
in
    type LineCapability = LineCapability
    val LineCapabilityE = encoding (W,fn W n => n)

    val LC_NONE                                      = W (0 (* Lines not supported *))
    val LC_POLYLINE                                  = W (2 (* Can do polylines *))
    val LC_MARKER                                    = W (4 (* Can do markers *))
    val LC_POLYMARKER                                = W (8 (* Can do polymarkers *))
    val LC_WIDE                                      = W (16 (* Can do wide lines *))
    val LC_STYLED                                    = W (32 (* Can do styled lines *))
    val LC_WIDESTYLED                                = W (64 (* Can do wide styled lines *))
    val LC_INTERIORS                                 = W (128 (* Can do interiors *))
end



(*------------------------------------------------------------------------------
INDEX: PolygonalCapability
------------------------------------------------------------------------------*)

(* GetDeviceCaps()  *)

(*TYPE: PolygonalCapability *)
local
    datatype PolygonalCapability =
    W of int
in
    type PolygonalCapability = PolygonalCapability
    val PolygonalCapabilityE = encoding (W,fn W n => n)

    val PC_NONE                                      = W (0 (* Polygonals not supported *))
    val PC_POLYGON                                   = W (1 (* Can do polygons *))
    val PC_RECTANGLE                                 = W (2 (* Can do rectangles *))
    val PC_WINDPOLYGON                               = W (4 (* Can do winding polygons *))
    val PC_TRAPEZOID                                 = W (4 (* Can do trapezoids *))
    val PC_SCANLINE                                  = W (8 (* Can do scanlines *))
    val PC_WIDE                                      = W (16 (* Can do wide borders *))
    val PC_STYLED                                    = W (32 (* Can do styled borders *))
    val PC_WIDESTYLED                                = W (64 (* Can do wide styled borders *))
    val PC_INTERIORS                                 = W (128 (* Can do interiors *))
end



(*------------------------------------------------------------------------------
INDEX: ClippingCapability
------------------------------------------------------------------------------*)

(* Polygonal capabilities *)
(* GetDeviceCaps()  *)

(*TYPE: ClippingCapability *)
local
    datatype ClippingCapability =
    W of int
in
    type ClippingCapability = ClippingCapability
    val ClippingCapabilityE = encoding (W,fn W n => n)

    val CP_NONE                                      = W (0 (* No clipping of output *))
    val CP_RECTANGLE                                 = W (1 (* Output clipped to rects *))
    val CP_REGION                                    = W (2 (* *))
end



(*------------------------------------------------------------------------------
INDEX: TextCapability
------------------------------------------------------------------------------*)

(* Text capabilities *)
(* GetDeviceCaps()  *)

(*TYPE: TextCapability *)
local
    datatype TextCapability =
    W of int
in
    type TextCapability = TextCapability
    val TextCapabilityE = encoding (W,fn W n => n)

    val TC_OP_CHARACTER                              = W (hex "0x00000001" (* Can do OutputPrecision CHARACTER *))
    val TC_OP_STROKE                                 = W (hex "0x00000002" (* Can do OutputPrecision STROKE *))
    val TC_CP_STROKE                                 = W (hex "0x00000004" (* Can do ClipPrecision STROKE *))
    val TC_CR_90                                     = W (hex "0x00000008" (* Can do CharRotAbility 90 *))
    val TC_CR_ANY                                    = W (hex "0x00000010" (* Can do CharRotAbility ANY *))
    val TC_SF_X_YINDEP                               = W (hex "0x00000020" (* Can do ScaleFreedom X_YINDEPENDENT *))
    val TC_SA_DOUBLE                                 = W (hex "0x00000040" (* Can do ScaleAbility DOUBLE *))
    val TC_SA_INTEGER                                = W (hex "0x00000080" (* Can do ScaleAbility INTEGER *))
    val TC_SA_CONTIN                                 = W (hex "0x00000100" (* Can do ScaleAbility CONTINUOUS *))
    val TC_EA_DOUBLE                                 = W (hex "0x00000200" (* Can do EmboldenAbility DOUBLE *))
    val TC_IA_ABLE                                   = W (hex "0x00000400" (* Can do ItalisizeAbility ABLE *))
    val TC_UA_ABLE                                   = W (hex "0x00000800" (* Can do UnderlineAbility ABLE *))
    val TC_SO_ABLE                                   = W (hex "0x00001000" (* Can do StrikeOutAbility ABLE *))
    val TC_RA_ABLE                                   = W (hex "0x00002000" (* Can do RasterFontAble ABLE *))
    val TC_VA_ABLE                                   = W (hex "0x00004000" (* Can do VectorFontAble ABLE *))
    val TC_RESERVED                                  = W (hex "0x00008000")
    val TC_SCROLLBLT                                 = W (hex "0x00010000" (* do text scroll with blt *))
end



(*------------------------------------------------------------------------------
INDEX: RasterCapability
------------------------------------------------------------------------------*)

(* GetDeviceCaps()  *)

(*TYPE: RasterCapability *)
local
    datatype RasterCapability =
    W of int
in
    type RasterCapability = RasterCapability
    val RasterCapabilityE = encoding (W,fn W n => n)

    val RC_BITBLT                                    = W (1 (* Can do standard BLT. *))
    val RC_BANDING                                   = W (2 (* Device requires banding support *))
    val RC_SCALING                                   = W (4 (* Device requires scaling support *))
    val RC_BITMAP64                                  = W (8 (* Device can support >64K bitmap *))
    val RC_GDI20_OUTPUT                              = W (hex "0x0010" (* has 2.0 output calls *))
    val RC_GDI20_STATE                               = W (hex "0x0020")
    val RC_SAVEBITMAP                                = W (hex "0x0040")
    val RC_DI_BITMAP                                 = W (hex "0x0080" (* supports DIB to memory *))
    val RC_PALETTE                                   = W (hex "0x0100" (* supports a palette *))
    val RC_DIBTODEV                                  = W (hex "0x0200" (* supports DIBitsToDevice *))
    val RC_BIGFONT                                   = W (hex "0x0400" (* supports >64K fonts *))
    val RC_STRETCHBLT                                = W (hex "0x0800" (* supports StretchBlt *))
    val RC_FLOODFILL                                 = W (hex "0x1000" (* supports FloodFill *))
    val RC_STRETCHDIB                                = W (hex "0x2000" (* supports StretchDIBits *))
    val RC_OP_DX_OUTPUT                              = W (hex "0x4000")
    val RC_DEVBITS                                   = W (hex "0x8000")
end



(*------------------------------------------------------------------------------
INDEX: DeviceIndependentPalette
------------------------------------------------------------------------------*)

(* DIB Color Table IDs *)
(* CreateDIBitmap()
   CreateDIBPatternBrush()
   CreateDIBPatternBrushPt()
   GetDIBits()
   SetDIBits()
   SetDIBitsToDevice()
   StretchDIBits()
 *)

(*TYPE: DeviceIndependentPalette *)
local
    datatype DeviceIndependentPalette =
    W of int
in
    type DeviceIndependentPalette = DeviceIndependentPalette
    val DeviceIndependentPaletteE = encoding (W,fn W n => n)

    val DIB_RGB_COLORS                               = W (0 (* color table in RGBs *))
    val DIB_PAL_COLORS                               = W (1 (* color table in palette indices *))
    val DIB_PAL_INDICES                              = W (2 (* No color table indices into surf palette *))
    val DIB_PAL_PHYSINDICES                          = W (2 (* No color table indices into surf palette *))
    val DIB_PAL_LOGINDICES                           = W (4 (* No color table indices into DC palette *))
end



(*------------------------------------------------------------------------------
INDEX: SystemPaletteUse
------------------------------------------------------------------------------*)

(* GetSystemPaletteUse()
   SetSystemPaletteUse()  *)

(*TYPE: SystemPaletteUse *)
local
    datatype SystemPaletteUse =
    W of int
in
    type SystemPaletteUse = SystemPaletteUse
    val SystemPaletteUseE = encoding (W,fn W n => n)

    val SYSPAL_ERROR                                 = W (0)
    val SYSPAL_STATIC                                = W (1)
    val SYSPAL_NOSTATIC                              = W (2)
end



(*------------------------------------------------------------------------------
INDEX: BitmapInit
------------------------------------------------------------------------------*)

(* Create bitmap *)
(* CreateDIBitmap()  *)

(*TYPE: BitmapInit *)
local
    datatype BitmapInit =
    W of int
in
    type BitmapInit = BitmapInit
    val BitmapInitE = encoding (W,fn W n => n)

    val CBM_CREATEDIB                                = W (hex "0x02" (* create DIB bitmap *))
    val CBM_INIT                                     = W (hex "0x04" (* initialize bitmap *))
end



(*------------------------------------------------------------------------------
INDEX: FloodFillMode
------------------------------------------------------------------------------*)

(* ExtFloodFill()  *)

(*TYPE: FloodFillMode *)
local
    datatype FloodFillMode =
    W of int
in
    type FloodFillMode = FloodFillMode
    val FloodFillModeE = encoding (W,fn W n => n)

    val FLOODFILLBORDER                              = W (0)
    val FLOODFILLSURFACE                             = W (1)
    val CCHDEVICENAME                                = W (32)
    val CCHFORMNAME                                  = W (32)
end



(*------------------------------------------------------------------------------
INDEX: DeviceMode
------------------------------------------------------------------------------*)

(*TYPE: DeviceMode *)
local
    datatype DeviceMode =
    W of int
in
    type DeviceMode = DeviceMode
    val DeviceModeE = encoding (W,fn W n => n)

    val DM_SPECVERSION                               = W (hex "0x320")
    val DM_ORIENTATION                               = W (hex "0x0000001")
    val DM_PAPERSIZE                                 = W (hex "0x0000002")
    val DM_PAPERLENGTH                               = W (hex "0x0000004")
    val DM_PAPERWIDTH                                = W (hex "0x0000008")
    val DM_SCALE                                     = W (hex "0x0000010")
    val DM_COPIES                                    = W (hex "0x0000100")
    val DM_DEFAULTSOURCE                             = W (hex "0x0000200")
    val DM_PRINTQUALITY                              = W (hex "0x0000400")
    val DM_COLOR                                     = W (hex "0x0000800")
    val DM_DUPLEX                                    = W (hex "0x0001000")
    val DM_YRESOLUTION                               = W (hex "0x0002000")
    val DM_TTOPTION                                  = W (hex "0x0004000")
    val DM_COLLATE                                   = W (hex "0x0008000")
    val DM_FORMNAME                                  = W (hex "0x0010000")
end



(*------------------------------------------------------------------------------
INDEX: PaperOrientation
------------------------------------------------------------------------------*)

(*TYPE: PaperOrientation *)
local
    datatype PaperOrientation =
    W of int
in
    type PaperOrientation = PaperOrientation
    val PaperOrientationE = encoding (W,fn W n => n)

    val DMORIENT_PORTRAIT                            = W (1)
    val DMORIENT_LANDSCAPE                           = W (2)
end



(*------------------------------------------------------------------------------
INDEX: PaperSize
------------------------------------------------------------------------------*)

(*TYPE: PaperSize *)
local
    datatype PaperSize =
    W of int
in
    type PaperSize = PaperSize
    val PaperSizeE = encoding (W,fn W n => n)

    val DMPAPER_LETTER                               = W (1 (* Letter 8 1/2 x 11 in *))
    val DMPAPER_FIRST                                = DMPAPER_LETTER
    val DMPAPER_LETTERSMALL                          = W (2 (* Letter Small 8 1/2 x 11 in *))
    val DMPAPER_TABLOID                              = W (3 (* Tabloid 11 x 17 in *))
    val DMPAPER_LEDGER                               = W (4 (* Ledger 17 x 11 in *))
    val DMPAPER_LEGAL                                = W (5 (* Legal 8 1/2 x 14 in *))
    val DMPAPER_STATEMENT                            = W (6 (* Statement 5 1/2 x 8 1/2 in *))
    val DMPAPER_EXECUTIVE                            = W (7 (* Executive 7 1/4 x 10 1/2 in *))
    val DMPAPER_A3                                   = W (8 (* A3 297 x 420 mm *))
    val DMPAPER_A4                                   = W (9 (* A4 210 x 297 mm *))
    val DMPAPER_A4SMALL                              = W (10 (* A4 Small 210 x 297 mm *))
    val DMPAPER_A5                                   = W (11 (* A5 148 x 210 mm *))
    val DMPAPER_B4                                   = W (12 (* B4 250 x 354 *))
    val DMPAPER_B5                                   = W (13 (* B5 182 x 257 mm *))
    val DMPAPER_FOLIO                                = W (14 (* Folio 8 1/2 x 13 in *))
    val DMPAPER_QUARTO                               = W (15 (* Quarto 215 x 275 mm *))
    val DMPAPER_10X14                                = W (16 (* 1hex "0x14" in *))
    val DMPAPER_11X17                                = W (17 (* 11x17 in *))
    val DMPAPER_NOTE                                 = W (18 (* Note 8 1/2 x 11 in *))
    val DMPAPER_ENV_9                                = W (19 (* Envelope #9 3 7/8 x 8 7/8 *))
    val DMPAPER_ENV_10                               = W (20 (* Envelope #10 4 1/8 x 9 1/2 *))
    val DMPAPER_ENV_11                               = W (21 (* Envelope #11 4 1/2 x 10 3/8 *))
    val DMPAPER_ENV_12                               = W (22 (* Envelope #12 4 \276 x 11 *))
    val DMPAPER_ENV_14                               = W (23 (* Envelope #14 5 x 11 1/2 *))
    val DMPAPER_CSHEET                               = W (24 (* C size sheet *))
    val DMPAPER_DSHEET                               = W (25 (* D size sheet *))
    val DMPAPER_ESHEET                               = W (26 (* E size sheet *))
    val DMPAPER_ENV_DL                               = W (27 (* Envelope DL 110 x 220mm *))
    val DMPAPER_ENV_C5                               = W (28 (* Envelope C5 162 x 229 mm *))
    val DMPAPER_ENV_C3                               = W (29 (* Envelope C3 324 x 458 mm *))
    val DMPAPER_ENV_C4                               = W (30 (* Envelope C4 229 x 324 mm *))
    val DMPAPER_ENV_C6                               = W (31 (* Envelope C6 114 x 162 mm *))
    val DMPAPER_ENV_C65                              = W (32 (* Envelope C65 114 x 229 mm *))
    val DMPAPER_ENV_B4                               = W (33 (* Envelope B4 250 x 353 mm *))
    val DMPAPER_ENV_B5                               = W (34 (* Envelope B5 176 x 250 mm *))
    val DMPAPER_ENV_B6                               = W (35 (* Envelope B6 176 x 125 mm *))
    val DMPAPER_ENV_ITALY                            = W (36 (* Envelope 110 x 230 mm *))
    val DMPAPER_ENV_MONARCH                          = W (37 (* Envelope Monarch 3.875 x 7.5 in *))
    val DMPAPER_ENV_PERSONAL                         = W (38 (* 6 3/4 Envelope 3 5/8 x 6 1/2 in *))
    val DMPAPER_FANFOLD_US                           = W (39 (* US Std Fanfold 14 7/8 x 11 in *))
    val DMPAPER_FANFOLD_STD_GERMAN                   = W (40 (* German Std Fanfold 8 1/2 x 12 in *))
    val DMPAPER_FANFOLD_LGL_GERMAN                   = W (41 (* German Legal Fanfold 8 1/2 x 13 in *))
    val DMPAPER_LAST                                 = DMPAPER_FANFOLD_LGL_GERMAN
    val DMPAPER_USER                                 = W (256)

    val DMPAPER_FIRST                                = DMPAPER_LETTER
end



(*------------------------------------------------------------------------------
INDEX: BinMode
------------------------------------------------------------------------------*)

(*TYPE: BinMode *)
local
    datatype BinMode =
    W of int
in
    type BinMode = BinMode
    val BinModeE = encoding (W,fn W n => n)

    val DMBIN_UPPER                                  = W (1)
    val DMBIN_FIRST                                  = DMBIN_UPPER
    val DMBIN_ONLYONE                                = W (1)
    val DMBIN_LOWER                                  = W (2)
    val DMBIN_MIDDLE                                 = W (3)
    val DMBIN_MANUAL                                 = W (4)
    val DMBIN_ENVELOPE                               = W (5)
    val DMBIN_ENVMANUAL                              = W (6)
    val DMBIN_AUTO                                   = W (7)
    val DMBIN_TRACTOR                                = W (8)
    val DMBIN_SMALLFMT                               = W (9)
    val DMBIN_LARGEFMT                               = W (10)
    val DMBIN_LARGECAPACITY                          = W (11)
    val DMBIN_CASSETTE                               = W (14)
    val DMBIN_LAST                                   = DMBIN_CASSETTE
    val DMBIN_USER                                   = W (256 (* device specific bins start here *))
end



(*------------------------------------------------------------------------------
INDEX: DevModeResolution
------------------------------------------------------------------------------*)

(*TYPE: DevModeResolution *)
local
    datatype DevModeResolution =
    W of int
in
    type DevModeResolution = DevModeResolution
    val DevModeResolutionE = encoding (W,fn W n => n)

    val DMRES_DRAFT                                  = W ((~1))
    val DMRES_LOW                                    = W ((~2))
    val DMRES_MEDIUM                                 = W ((~3))
    val DMRES_HIGH                                   = W ((~4))
end



(*------------------------------------------------------------------------------
INDEX: Printer specific constants - not implemented 
------------------------------------------------------------------------------*)

(*
val DMCOLOR_MONOCHROME                           = 1
val DMCOLOR_COLOR                                = 2

val DMDUP_SIMPLEX                                = 1
val DMDUP_VERTICAL                               = 2
val DMDUP_HORIZONTAL                             = 3

val DMTT_BITMAP                                  = 1 (* print TT fonts as graphics *)
val DMTT_DOWNLOAD                                = 2 (* download TT fonts as soft fonts *)
val DMTT_SUBDEV                                  = 3 (* substitute device fonts for TT fonts *)

val DMCOLLATE_FALSE                              = 0
val DMCOLLATE_TRUE                               = 1

val DM_GRAYSCALE                                 = hex "0x00000001"
val DM_INTERLACED                                = hex "0x00000002"
*)


(*------------------------------------------------------------------------------
INDEX: Constant used by RGNDATAHEADER 
------------------------------------------------------------------------------*)

val RDH_RECTANGLES                               = 1


(*------------------------------------------------------------------------------
INDEX: used by GetGlyphOutline - not implemented  
------------------------------------------------------------------------------*)
(*
val GGO_METRICS                                  = 0
val GGO_BITMAP                                   = 1
val GGO_NATIVE                                   = 2
*)

(*------------------------------------------------------------------------------
INDEX: used by TTPOLYGONHEADER - not implemented  
------------------------------------------------------------------------------*)
(*
val TT_POLYGON_TYPE                              = 24
val TT_PRIM_LINE                                 = 1
val TT_PRIM_QSPLINE                              = 2
val TT_AVAILABLE                                 = hex "0x0001"
val TT_ENABLED                                   = hex "0x0002"
*)


(*------------------------------------------------------------------------------
INDEX: DocumentMode
------------------------------------------------------------------------------*)

(* DocumentProperties()  *)

(*TYPE: DocumentMode *)
local
    datatype DocumentMode =
    W of int
in
    type DocumentMode = DocumentMode
    val DocumentModeE = encoding (W,fn W n => n)

    val DM_UPDATE                                    = W (1)
    val DM_COPY                                      = W (2)
    val DM_PROMPT                                    = W (4)
    val DM_MODIFY                                    = W (8)
    val DM_IN_BUFFER                                 = DM_MODIFY
    val DM_IN_PROMPT                                 = DM_PROMPT
    val DM_OUT_BUFFER                                = DM_COPY
    val DM_OUT_DEFAULT                               = DM_UPDATE
end



(*------------------------------------------------------------------------------
INDEX: DeviceCapability
------------------------------------------------------------------------------*)

(* DeviceCapabilities()  *)

(*TYPE: DeviceCapability *)
local
    datatype DeviceCapability =
    W of int
in
    type DeviceCapability = DeviceCapability
    val DeviceCapabilityE = encoding (W,fn W n => n)

    val DC_FIELDS                                    = W (1)
    val DC_PAPERS                                    = W (2)
    val DC_PAPERSIZE                                 = W (3)
    val DC_MINEXTENT                                 = W (4)
    val DC_MAXEXTENT                                 = W (5)
    val DC_BINS                                      = W (6)
    val DC_DUPLEX                                    = W (7)
    val DC_SIZE                                      = W (8)
    val DC_EXTRA                                     = W (9)
    val DC_VERSION                                   = W (10)
    val DC_DRIVER                                    = W (11)
    val DC_BINNAMES                                  = W (12)
    val DC_ENUMRESOLUTIONS                           = W (13)
    val DC_FILEDEPENDENCIES                          = W (14)
    val DC_TRUETYPE                                  = W (15)
    val DC_PAPERNAMES                                = W (16)
    val DC_ORIENTATION                               = W (17)
    val DC_COPIES                                    = W (18)
end


(*------------------------------------------------------------------------------
INDEX: ColorAdjustmentFlag
------------------------------------------------------------------------------*)
    
local
    datatype ColorAdjustmentFlag =
    W of int
in
    type ColorAdjustmentFlag = ColorAdjustmentFlag
    val ColorAdjustmentFlagE = encoding (W,fn W n => n)

    val CA_NEGATIVE                                  = W (hex "0x0001")
    val CA_LOG_FILTER                                = W (hex "0x0002")
end

(*------------------------------------------------------------------------------
INDEX: IlluminantConstant
------------------------------------------------------------------------------*)
    
local
    datatype IlluminantConstant =
    W of int
in
    type IlluminantConstant = IlluminantConstant
    val IlluminantConstantE = encoding (W,fn W n => n)

    val ILLUMINANT_DEVICE_DEFAULT                    = W (0)
    val ILLUMINANT_A                                 = W (1)
    val ILLUMINANT_B                                 = W (2)
    val ILLUMINANT_C                                 = W (3)
    val ILLUMINANT_D50                               = W (4)
    val ILLUMINANT_D55                               = W (5)
    val ILLUMINANT_D65                               = W (6)
    val ILLUMINANT_D75                               = W (7)
    val ILLUMINANT_F2                                = W (8)
    val ILLUMINANT_MAX_INDEX                         = ILLUMINANT_F2
    val ILLUMINANT_TUNGSTEN                          = ILLUMINANT_A
    val ILLUMINANT_DAYLIGHT                          = ILLUMINANT_C
    val ILLUMINANT_FLUORESCENT                       = ILLUMINANT_F2
    val ILLUMINANT_NTSC                              = ILLUMINANT_C
end

(*------------------------------------------------------------------------------
INDEX: Constants
------------------------------------------------------------------------------*)

val RGB_GAMMA_MIN                                = 02500
val RGB_GAMMA_MAX                                = 65000
val REFERENCE_WHITE_MIN                          = 6000
val REFERENCE_WHITE_MAX                          = 10000
val REFERENCE_BLACK_MIN                          = 0
val REFERENCE_BLACK_MAX                          = 4000
val COLOR_ADJ_MIN                                = ~100
val COLOR_ADJ_MAX                                = 100



val FONTMAPPER_MAX                               = 10


(* Enhanced metafiles - not implemented *)
(*

val ENHMETA_SIGNATURE                            = hex "0x464D4520"
val ENHMETA_STOCK_OBJECT                         = hex "0x80000000"

val EMR_HEADER                                   = 1
val EMR_POLYBEZIER                               = 2
val EMR_POLYGON                                  = 3
val EMR_POLYLINE                                 = 4
val EMR_POLYBEZIERTO                             = 5
val EMR_POLYLINETO                               = 6
val EMR_POLYPOLYLINE                             = 7
val EMR_POLYPOLYGON                              = 8
val EMR_SETWINDOWEXTEX                           = 9
val EMR_SETWINDOWORGEX                           = 10
val EMR_SETVIEWPORTEXTEX                         = 11
val EMR_SETVIEWPORTORGEX                         = 12
val EMR_SETBRUSHORGEX                            = 13
val EMR_EOF                                      = 14
val EMR_SETPIXELV                                = 15
val EMR_SETMAPPERFLAGS                           = 16
val EMR_SETMAPMODE                               = 17
val EMR_SETBKMODE                                = 18
val EMR_SETPOLYFILLMODE                          = 19
val EMR_SETROP2                                  = 20
val EMR_SETSTRETCHBLTMODE                        = 21
val EMR_SETTEXTALIGN                             = 22
val EMR_SETCOLORADJUSTMENT                       = 23
val EMR_SETTEXTCOLOR                             = 24
val EMR_SETBKCOLOR                               = 25
val EMR_OFFSETCLIPRGN                            = 26
val EMR_MOVETOEX                                 = 27
val EMR_SETMETARGN                               = 28
val EMR_EXCLUDECLIPRECT                          = 29
val EMR_INTERSECTCLIPRECT                        = 30
val EMR_SCALEVIEWPORTEXTEX                       = 31
val EMR_SCALEWINDOWEXTEX                         = 32
val EMR_SAVEDC                                   = 33
val EMR_RESTOREDC                                = 34
val EMR_SETWORLDTRANSFORM                        = 35
val EMR_MODIFYWORLDTRANSFORM                     = 36
val EMR_SELECTOBJECT                             = 37
val EMR_CREATEPEN                                = 38
val EMR_CREATEBRUSHINDIRECT                      = 39
val EMR_DELETEOBJECT                             = 40
val EMR_ANGLEARC                                 = 41
val EMR_ELLIPSE                                  = 42
val EMR_RECTANGLE                                = 43
val EMR_ROUNDRECT                                = 44
val EMR_ARC                                      = 45
val EMR_CHORD                                    = 46
val EMR_PIE                                      = 47
val EMR_SELECTPALETTE                            = 48
val EMR_CREATEPALETTE                            = 49
val EMR_SETPALETTEENTRIES                        = 50
val EMR_RESIZEPALETTE                            = 51
val EMR_REALIZEPALETTE                           = 52
val EMR_EXTFLOODFILL                             = 53
val EMR_LINETO                                   = 54
val EMR_ARCTO                                    = 55
val EMR_POLYDRAW                                 = 56
val EMR_SETARCDIRECTION                          = 57
val EMR_SETMITERLIMIT                            = 58
val EMR_BEGINPATH                                = 59
val EMR_ENDPATH                                  = 60
val EMR_CLOSEFIGURE                              = 61
val EMR_FILLPATH                                 = 62
val EMR_STROKEANDFILLPATH                        = 63
val EMR_STROKEPATH                               = 64
val EMR_FLATTENPATH                              = 65
val EMR_WIDENPATH                                = 66
val EMR_SELECTCLIPPATH                           = 67
val EMR_ABORTPATH                                = 68
val EMR_GDICOMMENT                               = 70
val EMR_FILLRGN                                  = 71
val EMR_FRAMERGN                                 = 72
val EMR_INVERTRGN                                = 73
val EMR_PAINTRGN                                 = 74
val EMR_EXTSELECTCLIPRGN                         = 75
val EMR_BITBLT                                   = 76
val EMR_STRETCHBLT                               = 77
val EMR_MASKBLT                                  = 78
val EMR_PLGBLT                                   = 79
val EMR_SETDIBITSTODEVICE                        = 80
val EMR_STRETCHDIBITS                            = 81
val EMR_EXTCREATEFONTINDIRECTW                   = 82
val EMR_EXTTEXTOUTA                              = 83
val EMR_EXTTEXTOUTW                              = 84
val EMR_POLYBEZIER16                             = 85
val EMR_POLYGON16                                = 86
val EMR_POLYLINE16                               = 87
val EMR_POLYBEZIERTO16                           = 88
val EMR_POLYLINETO16                             = 89
val EMR_POLYPOLYLINE16                           = 90
val EMR_POLYPOLYGON16                            = 91
val EMR_POLYDRAW16                               = 92
val EMR_CREATEMONOBRUSH                          = 93
val EMR_CREATEDIBPATTERNBRUSHPT                  = 94
val EMR_EXTCREATEPEN                             = 95
val EMR_POLYTEXTOUTA                             = 96
val EMR_POLYTEXTOUTW                             = 97
val EMR_MIN                                      = 1
val EMR_MAX                                      = 97

*)


(*******************************************
 *
 * CONST for winuser (grouped)
 *
 *******************************************)


(*------------------------------------------------------------------------------
INDEX: ResourceType
------------------------------------------------------------------------------*)

(* LookupIconFromDirectory() 
   EnumResourceLanguages()
   EnumResourceNames()
   FindResource()
   FindResourceEx()
   UpdateResource()  *)

(*TYPE: ResourceType *)
local
    datatype ResourceType =
    W of int
in
    type ResourceType = ResourceType
    val ResourceTypeE = encoding (W,fn W n => n)

    val RT_CURSOR                                    = W (MAKEINTRESOURCE(1))
    val RT_BITMAP                                    = W (MAKEINTRESOURCE(2))
    val RT_ICON                                      = W (MAKEINTRESOURCE(3))
    val RT_MENU                                      = W (MAKEINTRESOURCE(4))
    val RT_DIALOG                                    = W (MAKEINTRESOURCE(5))
    val RT_STRING                                    = W (MAKEINTRESOURCE(6))
    val RT_FONTDIR                                   = W (MAKEINTRESOURCE(7))
    val RT_FONT                                      = W (MAKEINTRESOURCE(8))
    val RT_ACCELERATOR                               = W (MAKEINTRESOURCE(9))
    val RT_RCDATA                                    = W (MAKEINTRESOURCE(10))
    val RT_MESSAGETABLE                              = W (MAKEINTRESOURCE(11))
    val RT_GROUP_CURSOR                              = addLE ResourceTypeE [RT_CURSOR,W DIFFERENCE]
    val RT_GROUP_ICON                                = addLE ResourceTypeE [RT_ICON,W DIFFERENCE]
    val RT_VERSION                                   = W (MAKEINTRESOURCE(16))
    val RT_DLGINCLUDE                                = W (MAKEINTRESOURCE(17))
end



(*------------------------------------------------------------------------------
INDEX: ScrollBarType
------------------------------------------------------------------------------*)

(* Scroll Bar *)

(*TYPE: ScrollBarType *)
local
    datatype ScrollBarType =
    W of int
in
    type ScrollBarType = ScrollBarType
    val ScrollBarTypeE = encoding (W,fn W n => n)

    val SB_HORZ                                      = W (0)
    val SB_VERT                                      = W (1)
    val SB_CTL                                       = W (2)
    val SB_BOTH                                      = W (3)
    val SB_LINEUP                                    = W (0)
    val SB_LINELEFT                                  = W (0)
    val SB_LINEDOWN                                  = W (1)
    val SB_LINERIGHT                                 = W (1)
    val SB_PAGEUP                                    = W (2)
    val SB_PAGELEFT                                  = W (2)
    val SB_PAGEDOWN                                  = W (3)
    val SB_PAGERIGHT                                 = W (3)
    val SB_THUMBPOSITION                             = W (4)
    val SB_THUMBTRACK                                = W (5)
    val SB_TOP                                       = W (6)
    val SB_LEFT                                      = W (6)
    val SB_BOTTOM                                    = W (7)
    val SB_RIGHT                                     = W (7)
    val SB_ENDSCROLL                                 = W (8)
end



(*------------------------------------------------------------------------------
INDEX: ShowWindowMode
------------------------------------------------------------------------------*)

(* Scroll messages *)
(* ShowWindow()  *)

(*TYPE: ShowWindowMode *)
local
    datatype ShowWindowMode =
    W of int
in
    type ShowWindowMode = ShowWindowMode
    val ShowWindowModeE = encoding (W,fn W n => n)

    val SW_HIDE                                      = W (0)
    val SW_SHOWNORMAL                                = W (1)
    val SW_NORMAL                                    = W (1)
    val SW_SHOWMINIMIZED                             = W (2)
    val SW_SHOWMAXIMIZED                             = W (3)
    val SW_MAXIMIZE                                  = W (3)
    val SW_SHOWNOACTIVATE                            = W (4)
    val SW_SHOW                                      = W (5)
    val SW_MINIMIZE                                  = W (6)
    val SW_SHOWMINNOACTIVE                           = W (7)
    val SW_SHOWNA                                    = W (8)
    val SW_RESTORE                                   = W (9)
    val SW_SHOWDEFAULT                               = W (10)
    val SW_MAX                                       = W (10)
    val HIDE_WINDOW                                  = W (0)
    val SHOW_OPENWINDOW                              = W (1)
    val SHOW_ICONWINDOW                              = W (2)
    val SHOW_FULLSCREEN                              = W (3)
    val SHOW_OPENNOACTIVATE                          = W (4)
    val SW_PARENTCLOSING                             = W (1)
    val SW_OTHERZOOM                                 = W (2)
    val SW_PARENTOPENING                             = W (3)
    val SW_OTHERUNZOOM                               = W (4)
end



(*------------------------------------------------------------------------------
INDEX: KeyFunction
------------------------------------------------------------------------------*)

(*TYPE: KeyFunction *)
local
    datatype KeyFunction =
    W of int
in
    type KeyFunction = KeyFunction
    val KeyFunctionE = encoding (W,fn W n => n)

    val KF_EXTENDED                                  = W (hex "0x0100")
    val KF_DLGMODE                                   = W (hex "0x0800")
    val KF_MENUMODE                                  = W (hex "0x1000")
    val KF_ALTDOWN                                   = W (hex "0x2000")
    val KF_REPEAT                                    = W (hex "0x4000")
    val KF_UP                                        = W (hex "0x8000")
end



(*------------------------------------------------------------------------------
INDEX: VirtualKey
------------------------------------------------------------------------------*)

(* Virtual Keys *)

(*TYPE: VirtualKey *)
local
    datatype VirtualKey =
    W of int
in
    type VirtualKey = VirtualKey
    val VirtualKeyE = encoding (W,fn W n => n)

    val VK_LBUTTON                                   = W (hex "0x01")
    val VK_RBUTTON                                   = W (hex "0x02")
    val VK_CANCEL                                    = W (hex "0x03")
    val VK_MBUTTON                                   = W (hex "0x04" (* NOT contiguous with L & RBUTTON *))
    val VK_BACK                                      = W (hex "0x08")
    val VK_TAB                                       = W (hex "0x09")
    val VK_CLEAR                                     = W (hex "0x0C")
    val VK_RETURN                                    = W (hex "0x0D")
    val VK_SHIFT                                     = W (hex "0x10")
    val VK_CONTROL                                   = W (hex "0x11")
    val VK_MENU                                      = W (hex "0x12")
    val VK_PAUSE                                     = W (hex "0x13")
    val VK_CAPITAL                                   = W (hex "0x14")
    val VK_ESCAPE                                    = W (hex "0x1B")
    val VK_SPACE                                     = W (hex "0x20")
    val VK_PRIOR                                     = W (hex "0x21")
    val VK_NEXT                                      = W (hex "0x22")
    val VK_END                                       = W (hex "0x23")
    val VK_HOME                                      = W (hex "0x24")
    val VK_LEFT                                      = W (hex "0x25")
    val VK_UP                                        = W (hex "0x26")
    val VK_RIGHT                                     = W (hex "0x27")
    val VK_DOWN                                      = W (hex "0x28")
    val VK_SELECT                                    = W (hex "0x29")
    val VK_PRINT                                     = W (hex "0x2A")
    val VK_EXECUTE                                   = W (hex "0x2B")
    val VK_SNAPSHOT                                  = W (hex "0x2C")
    val VK_INSERT                                    = W (hex "0x2D")
    val VK_DELETE                                    = W (hex "0x2E")
    val VK_HELP                                      = W (hex "0x2F")
    val VK_NUMPAD0                                   = W (hex "0x60")
    val VK_NUMPAD1                                   = W (hex "0x61")
    val VK_NUMPAD2                                   = W (hex "0x62")
    val VK_NUMPAD3                                   = W (hex "0x63")
    val VK_NUMPAD4                                   = W (hex "0x64")
    val VK_NUMPAD5                                   = W (hex "0x65")
    val VK_NUMPAD6                                   = W (hex "0x66")
    val VK_NUMPAD7                                   = W (hex "0x67")
    val VK_NUMPAD8                                   = W (hex "0x68")
    val VK_NUMPAD9                                   = W (hex "0x69")
    val VK_MULTIPLY                                  = W (hex "0x6A")
    val VK_ADD                                       = W (hex "0x6B")
    val VK_SEPARATOR                                 = W (hex "0x6C")
    val VK_SUBTRACT                                  = W (hex "0x6D")
    val VK_DECIMAL                                   = W (hex "0x6E")
    val VK_DIVIDE                                    = W (hex "0x6F")
    val VK_F1                                        = W (hex "0x70")
    val VK_F2                                        = W (hex "0x71")
    val VK_F3                                        = W (hex "0x72")
    val VK_F4                                        = W (hex "0x73")
    val VK_F5                                        = W (hex "0x74")
    val VK_F6                                        = W (hex "0x75")
    val VK_F7                                        = W (hex "0x76")
    val VK_F8                                        = W (hex "0x77")
    val VK_F9                                        = W (hex "0x78")
    val VK_F10                                       = W (hex "0x79")
    val VK_F11                                       = W (hex "0x7A")
    val VK_F12                                       = W (hex "0x7B")
    val VK_F13                                       = W (hex "0x7C")
    val VK_F14                                       = W (hex "0x7D")
    val VK_F15                                       = W (hex "0x7E")
    val VK_F16                                       = W (hex "0x7F")
    val VK_F17                                       = W (hex "0x80")
    val VK_F18                                       = W (hex "0x81")
    val VK_F19                                       = W (hex "0x82")
    val VK_F20                                       = W (hex "0x83")
    val VK_F21                                       = W (hex "0x84")
    val VK_F22                                       = W (hex "0x85")
    val VK_F23                                       = W (hex "0x86")
    val VK_F24                                       = W (hex "0x87")
    val VK_NUMLOCK                                   = W (hex "0x90")
    val VK_SCROLL                                    = W (hex "0x91")
    val VK_LSHIFT                                    = W (hex "0xA0")
    val VK_RSHIFT                                    = W (hex "0xA1")
    val VK_LCONTROL                                  = W (hex "0xA2")
    val VK_RCONTROL                                  = W (hex "0xA3")
    val VK_LMENU                                     = W (hex "0xA4")
    val VK_RMENU                                     = W (hex "0xA5")
    val VK_ATTN                                      = W (hex "0xF6")
    val VK_CRSEL                                     = W (hex "0xF7")
    val VK_EXSEL                                     = W (hex "0xF8")
    val VK_EREOF                                     = W (hex "0xF9")
    val VK_PLAY                                      = W (hex "0xFA")
    val VK_ZOOM                                      = W (hex "0xFB")
    val VK_NONAME                                    = W (hex "0xFC")
    val VK_PA1                                       = W (hex "0xFD")
    val VK_OEM_CLEAR                                 = W (hex "0xFE")
end



(*------------------------------------------------------------------------------
INDEX: WindowHook
------------------------------------------------------------------------------*)

(* Hooks and other procs *)

(*TYPE: WindowHook *)
local
    datatype WindowHook =
    W of int
in
    type WindowHook = WindowHook
    val WindowHookE = encoding (W,fn W n => n)

    val WH_MIN                                       = W ((~1))
    val WH_MSGFILTER                                 = W ((~1))
    val WH_JOURNALRECORD                             = W (0)
    val WH_JOURNALPLAYBACK                           = W (1)
    val WH_KEYBOARD                                  = W (2)
    val WH_GETMESSAGE                                = W (3)
    val WH_CALLWNDPROC                               = W (4)
    val WH_CBT                                       = W (5)
    val WH_SYSMSGFILTER                              = W (6)
    val WH_MOUSE                                     = W (7)
    val WH_HARDWARE                                  = W (8)
    val WH_DEBUG                                     = W (9)
    val WH_SHELL                                     = W (10)
    val WH_FOREGROUNDIDLE                            = W (11)
    val WH_MAX                                       = W (11)
end



(*------------------------------------------------------------------------------
INDEX: HookCode
------------------------------------------------------------------------------*)

(*TYPE: HookCode *)
local
    datatype HookCode =
    W of int
in
    type HookCode = HookCode
    val HookCodeE = encoding (W,fn W n => n)

    val HC_ACTION                                    = W (0)
    val HC_GETNEXT                                   = W (1)
    val HC_SKIP                                      = W (2)
    val HC_NOREMOVE                                  = W (3)
    val HC_NOREM                                     = HC_NOREMOVE
    val HC_SYSMODALON                                = W (4)
    val HC_SYSMODALOFF                               = W (5)
end



(*------------------------------------------------------------------------------
INDEX: HookProcedureCode
------------------------------------------------------------------------------*)

(* Callback proc *)

(*TYPE: HookProcedureCode *)
local
    datatype HookProcedureCode =
    W of int
in
    type HookProcedureCode = HookProcedureCode
    val HookProcedureCodeE = encoding (W,fn W n => n)

    val HCBT_MOVESIZE                                = W (0)
    val HCBT_MINMAX                                  = W (1)
    val HCBT_QS                                      = W (2)
    val HCBT_CREATEWND                               = W (3)
    val HCBT_DESTROYWND                              = W (4)
    val HCBT_ACTIVATE                                = W (5)
    val HCBT_CLICKSKIPPED                            = W (6)
    val HCBT_KEYSKIPPED                              = W (7)
    val HCBT_SYSCOMMAND                              = W (8)
    val HCBT_SETFOCUS                                = W (9)
end



(*------------------------------------------------------------------------------
INDEX: InputEventType
------------------------------------------------------------------------------*)

(*TYPE: InputEventType *)
local
    datatype InputEventType =
    W of int
in
    type InputEventType = InputEventType
    val InputEventTypeE = encoding (W,fn W n => n)

    val MSGF_DIALOGBOX                               = W (0)
    val MSGF_MESSAGEBOX                              = W (1)
    val MSGF_MENU                                    = W (2)
    val MSGF_MOVE                                    = W (3)
    val MSGF_SIZE                                    = W (4)
    val MSGF_SCROLLBAR                               = W (5)
    val MSGF_NEXTWINDOW                              = W (6)
    val MSGF_MAINLOOP                                = W (8)
    val MSGF_MAX                                     = W (8)
    val MSGF_USER                                    = W (4096)
end


(*------------------------------------------------------------------------------
INDEX: KeyLayoutFlag
------------------------------------------------------------------------------*)

(* LoadKeyboardLayout()
   ActivateKeyboardLayout()  *)

(*TYPE: KeyLayoutFlag *)
local
    datatype KeyLayoutFlag =
    W of int
in
    type KeyLayoutFlag = KeyLayoutFlag
    val KeyLayoutFlagE = encoding (W,fn W n => n)

    val KLF_ACTIVATE                                 = W (hex "0x00000001")
    val KLF_SUBSTITUTE_OK                            = W (hex "0x00000002")
    val KLF_UNLOADPREVIOUS                           = W (hex "0x00000004")
    val KLF_REORDER                                  = W (hex "0x00000008")
    val KL_NAMELENGTH                                = W (9)
end



(*------------------------------------------------------------------------------
INDEX: DesktopOperation
------------------------------------------------------------------------------*)

(*TYPE: DesktopOperation *)
local
    datatype DesktopOperation =
    W of int
in
    type DesktopOperation = DesktopOperation
    val DesktopOperationE = encoding (W,fn W n => n)

    val DESKTOP_READOBJECTS                          = W (hex "0x0001")
    val DESKTOP_CREATEWINDOW                         = W (hex "0x0002")
    val DESKTOP_CREATEMENU                           = W (hex "0x0004")
    val DESKTOP_HOOKCONTROL                          = W (hex "0x0008")
    val DESKTOP_JOURNALRECORD                        = W (hex "0x0010")
    val DESKTOP_JOURNALPLAYBACK                      = W (hex "0x0020")
    val DESKTOP_ENUMERATE                            = W (hex "0x0040")
    val DESKTOP_WRITEOBJECTS                         = W (hex "0x0080")
end



(*------------------------------------------------------------------------------
INDEX: WindowStationAccess
------------------------------------------------------------------------------*)

(*TYPE: WindowStationAccess *)
local
    datatype WindowStationAccess =
    W of int
in
    type WindowStationAccess = WindowStationAccess
    val WindowStationAccessE = encoding (W,fn W n => n)

    val WINSTA_ENUMDESKTOPS                          = W (hex "0x0001")
    val WINSTA_READATTRIBUTES                        = W (hex "0x0002")
    val WINSTA_ACCESSCLIPBOARD                       = W (hex "0x0004")
    val WINSTA_CREATEDESKTOP                         = W (hex "0x0008")
    val WINSTA_WRITEATTRIBUTES                       = W (hex "0x0010")
    val WINSTA_ACCESSGLOBALATOMS                     = W (hex "0x0020")
    val WINSTA_EXITWINDOWS                           = W (hex "0x0040")
    val WINSTA_ENUMERATE                             = W (hex "0x0100")
    val WINSTA_READSCREEN                            = W (hex "0x0200")
end



(*------------------------------------------------------------------------------
INDEX: GetWindowLongParam
------------------------------------------------------------------------------*)

(* GetWindowLong()
   SetWindowLong()  *)

(*TYPE: GetWindowLongParam *)
local
    datatype GetWindowLongParam =
    W of int
in
    type GetWindowLongParam = GetWindowLongParam
    val GetWindowLongParamE = encoding (W,fn W n => n)

    val GWL_WNDPROC                                  = W ((~4))
    val GWL_HINSTANCE                                = W ((~6))
    val GWL_HWNDPARENT                               = W ((~8))
    val GWL_STYLE                                    = W ((~16))
    val GWL_EXSTYLE                                  = W ((~20))
    val GWL_USERDATA                                 = W ((~21))
    val GWL_ID                                       = W ((~12))
end



(*------------------------------------------------------------------------------
INDEX: GetWindowLongValue
------------------------------------------------------------------------------*)

(* GetWindowLong()
   SetWindowLong()  *)

(*TYPE: GetWindowLongValue *)
local
    datatype GetWindowLongValue =
    W of int
in
    type GetWindowLongValue = GetWindowLongValue
    val GetWindowLongValueE = encoding (W,fn W n => n)

    val GCL_MENUNAME                                 = W ((~8))
    val GCL_HBRBACKGROUND                            = W ((~10))
    val GCL_HCURSOR                                  = W ((~12))
    val GCL_HICON                                    = W ((~14))
    val GCL_HMODULE                                  = W ((~16))
    val GCL_CBWNDEXTRA                               = W ((~18))
    val GCL_CBCLSEXTRA                               = W ((~20))
    val GCL_WNDPROC                                  = W ((~24))
    val GCL_STYLE                                    = W ((~26))
    val GCW_ATOM                                     = W ((~32))
end



(*------------------------------------------------------------------------------
INDEX: WindowActivationState
------------------------------------------------------------------------------*)

(* SetActiveWindow() *)

(*TYPE: WindowActivationState *)
local
    datatype WindowActivationState =
    W of int
in
    type WindowActivationState = WindowActivationState
    val WindowActivationStateE = encoding (W,fn W n => n)

    val WA_INACTIVE                                  = W (0)
    val WA_ACTIVE                                    = W (1)
    val WA_CLICKACTIVE                               = W (2)
end



(*------------------------------------------------------------------------------
INDEX: PowerState
------------------------------------------------------------------------------*)

(* Power *)

(*TYPE: PowerState *)
local
    datatype PowerState =
    W of int
in
    type PowerState = PowerState
    val PowerStateE = encoding (W,fn W n => n)

    val PWR_OK                                       = W (1)
    val PWR_FAIL                                     = W ((~1))
    val PWR_SUSPENDREQUEST                           = W (1)
    val PWR_SUSPENDRESUME                            = W (2)
    val PWR_CRITICALRESUME                           = W (3)
end



(*------------------------------------------------------------------------------
INDEX: WindowMessage
------------------------------------------------------------------------------*)

(* Window Messages *)

(*TYPE: WindowMessage *)
local
    datatype WindowMessage =
    W of int
in
    type WindowMessage = WindowMessage
    val WindowMessageE = encoding (W,fn W n => n)

    val WM_NULL                                      = W (hex "0x0000")
    val WM_CREATE                                    = W (hex "0x0001")
    val WM_DESTROY                                   = W (hex "0x0002")
    val WM_MOVE                                      = W (hex "0x0003")
    val WM_SIZE                                      = W (hex "0x0005")
    val WM_ACTIVATE                                  = W (hex "0x0006")
    val WM_SETFOCUS                                  = W (hex "0x0007")
    val WM_KILLFOCUS                                 = W (hex "0x0008")
    val WM_ENABLE                                    = W (hex "0x000A")
    val WM_SETREDRAW                                 = W (hex "0x000B")
    val WM_SETTEXT                                   = W (hex "0x000C")
    val WM_GETTEXT                                   = W (hex "0x000D")
    val WM_GETTEXTLENGTH                             = W (hex "0x000E")
    val WM_PAINT                                     = W (hex "0x000F")
    val WM_CLOSE                                     = W (hex "0x0010")
    val WM_QUERYENDSESSION                           = W (hex "0x0011")
    val WM_QUIT                                      = W (hex "0x0012")
    val WM_QUERYOPEN                                 = W (hex "0x0013")
    val WM_ERASEBKGND                                = W (hex "0x0014")
    val WM_SYSCOLORCHANGE                            = W (hex "0x0015")
    val WM_ENDSESSION                                = W (hex "0x0016")
    val WM_SHOWWINDOW                                = W (hex "0x0018")
    val WM_WININICHANGE                              = W (hex "0x001A")
    val WM_DEVMODECHANGE                             = W (hex "0x001B")
    val WM_ACTIVATEAPP                               = W (hex "0x001C")
    val WM_FONTCHANGE                                = W (hex "0x001D")
    val WM_TIMECHANGE                                = W (hex "0x001E")
    val WM_CANCELMODE                                = W (hex "0x001F")
    val WM_SETCURSOR                                 = W (hex "0x0020")
    val WM_MOUSEACTIVATE                             = W (hex "0x0021")
    val WM_CHILDACTIVATE                             = W (hex "0x0022")
    val WM_QUEUESYNC                                 = W (hex "0x0023")
    val WM_GETMINMAXINFO                             = W (hex "0x0024")
    val WM_PAINTICON                                 = W (hex "0x0026")
    val WM_ICONERASEBKGND                            = W (hex "0x0027")
    val WM_NEXTDLGCTL                                = W (hex "0x0028")
    val WM_SPOOLERSTATUS                             = W (hex "0x002A")
    val WM_DRAWITEM                                  = W (hex "0x002B")
    val WM_MEASUREITEM                               = W (hex "0x002C")
    val WM_DELETEITEM                                = W (hex "0x002D")
    val WM_VKEYTOITEM                                = W (hex "0x002E")
    val WM_CHARTOITEM                                = W (hex "0x002F")
    val WM_SETFONT                                   = W (hex "0x0030")
    val WM_GETFONT                                   = W (hex "0x0031")
    val WM_SETHOTKEY                                 = W (hex "0x0032")
    val WM_GETHOTKEY                                 = W (hex "0x0033")
    val WM_QUERYDRAGICON                             = W (hex "0x0037")
    val WM_COMPAREITEM                               = W (hex "0x0039")
    val WM_COMPACTING                                = W (hex "0x0041")
    val WM_OTHERWINDOWCREATED                        = W (hex "0x0042" (* no longer suported *))
    val WM_OTHERWINDOWDESTROYED                      = W (hex "0x0043" (* no longer suported *))
    val WM_COMMNOTIFY                                = W (hex "0x0044" (* no longer suported *))
    val WM_WINDOWPOSCHANGING                         = W (hex "0x0046")
    val WM_WINDOWPOSCHANGED                          = W (hex "0x0047")
    val WM_POWER                                     = W (hex "0x0048")
    val WM_COPYDATA                                  = W (hex "0x004A")
    val WM_CANCELJOURNAL                             = W (hex "0x004B")
    val WM_NCCREATE                                  = W (hex "0x0081")
    val WM_NCDESTROY                                 = W (hex "0x0082")
    val WM_NCCALCSIZE                                = W (hex "0x0083")
    val WM_NCHITTEST                                 = W (hex "0x0084")
    val WM_NCPAINT                                   = W (hex "0x0085")
    val WM_NCACTIVATE                                = W (hex "0x0086")
    val WM_GETDLGCODE                                = W (hex "0x0087")
    val WM_NCMOUSEMOVE                               = W (hex "0x00A0")
    val WM_NCLBUTTONDOWN                             = W (hex "0x00A1")
    val WM_NCLBUTTONUP                               = W (hex "0x00A2")
    val WM_NCLBUTTONDBLCLK                           = W (hex "0x00A3")
    val WM_NCRBUTTONDOWN                             = W (hex "0x00A4")
    val WM_NCRBUTTONUP                               = W (hex "0x00A5")
    val WM_NCRBUTTONDBLCLK                           = W (hex "0x00A6")
    val WM_NCMBUTTONDOWN                             = W (hex "0x00A7")
    val WM_NCMBUTTONUP                               = W (hex "0x00A8")
    val WM_NCMBUTTONDBLCLK                           = W (hex "0x00A9")
    val WM_KEYFIRST                                  = W (hex "0x0100")
    val WM_KEYDOWN                                   = W (hex "0x0100")
    val WM_KEYUP                                     = W (hex "0x0101")
    val WM_CHAR                                      = W (hex "0x0102")
    val WM_DEADCHAR                                  = W (hex "0x0103")
    val WM_SYSKEYDOWN                                = W (hex "0x0104")
    val WM_SYSKEYUP                                  = W (hex "0x0105")
    val WM_SYSCHAR                                   = W (hex "0x0106")
    val WM_SYSDEADCHAR                               = W (hex "0x0107")
    val WM_KEYLAST                                   = W (hex "0x0108")
    val WM_INITDIALOG                                = W (hex "0x0110")
    val WM_COMMAND                                   = W (hex "0x0111")
    val WM_SYSCOMMAND                                = W (hex "0x0112")
    val WM_TIMER                                     = W (hex "0x0113")
    val WM_HSCROLL                                   = W (hex "0x0114")
    val WM_VSCROLL                                   = W (hex "0x0115")
    val WM_INITMENU                                  = W (hex "0x0116")
    val WM_INITMENUPOPUP                             = W (hex "0x0117")
    val WM_MENUSELECT                                = W (hex "0x011F")
    val WM_MENUCHAR                                  = W (hex "0x0120")
    val WM_ENTERIDLE                                 = W (hex "0x0121")
    val WM_CTLCOLORMSGBOX                            = W (hex "0x0132")
    val WM_CTLCOLOREDIT                              = W (hex "0x0133")
    val WM_CTLCOLORLISTBOX                           = W (hex "0x0134")
    val WM_CTLCOLORBTN                               = W (hex "0x0135")
    val WM_CTLCOLORDLG                               = W (hex "0x0136")
    val WM_CTLCOLORSCROLLBAR                         = W (hex "0x0137")
    val WM_CTLCOLORSTATIC                            = W (hex "0x0138")
    val WM_MOUSEFIRST                                = W (hex "0x0200")
    val WM_MOUSEMOVE                                 = W (hex "0x0200")
    val WM_LBUTTONDOWN                               = W (hex "0x0201")
    val WM_LBUTTONUP                                 = W (hex "0x0202")
    val WM_LBUTTONDBLCLK                             = W (hex "0x0203")
    val WM_RBUTTONDOWN                               = W (hex "0x0204")
    val WM_RBUTTONUP                                 = W (hex "0x0205")
    val WM_RBUTTONDBLCLK                             = W (hex "0x0206")
    val WM_MBUTTONDOWN                               = W (hex "0x0207")
    val WM_MBUTTONUP                                 = W (hex "0x0208")
    val WM_MBUTTONDBLCLK                             = W (hex "0x0209")
    val WM_MOUSELAST                                 = W (hex "0x0209")
    val WM_PARENTNOTIFY                              = W (hex "0x0210")
    val WM_ENTERMENULOOP                             = W (hex "0x0211")
    val WM_EXITMENULOOP                              = W (hex "0x0212")
    val WM_MDICREATE                                 = W (hex "0x0220")
    val WM_MDIDESTROY                                = W (hex "0x0221")
    val WM_MDIACTIVATE                               = W (hex "0x0222")
    val WM_MDIRESTORE                                = W (hex "0x0223")
    val WM_MDINEXT                                   = W (hex "0x0224")
    val WM_MDIMAXIMIZE                               = W (hex "0x0225")
    val WM_MDITILE                                   = W (hex "0x0226")
    val WM_MDICASCADE                                = W (hex "0x0227")
    val WM_MDIICONARRANGE                            = W (hex "0x0228")
    val WM_MDIGETACTIVE                              = W (hex "0x0229")
    val WM_MDISETMENU                                = W (hex "0x0230")
    val WM_DROPFILES                                 = W (hex "0x0233")
    val WM_MDIREFRESHMENU                            = W (hex "0x0234")
    val WM_CUT                                       = W (hex "0x0300")
    val WM_COPY                                      = W (hex "0x0301")
    val WM_PASTE                                     = W (hex "0x0302")
    val WM_CLEAR                                     = W (hex "0x0303")
    val WM_UNDO                                      = W (hex "0x0304")
    val WM_RENDERFORMAT                              = W (hex "0x0305")
    val WM_RENDERALLFORMATS                          = W (hex "0x0306")
    val WM_DESTROYCLIPBOARD                          = W (hex "0x0307")
    val WM_DRAWCLIPBOARD                             = W (hex "0x0308")
    val WM_PAINTCLIPBOARD                            = W (hex "0x0309")
    val WM_VSCROLLCLIPBOARD                          = W (hex "0x030A")
    val WM_SIZECLIPBOARD                             = W (hex "0x030B")
    val WM_ASKCBFORMATNAME                           = W (hex "0x030C")
    val WM_CHANGECBCHAIN                             = W (hex "0x030D")
    val WM_HSCROLLCLIPBOARD                          = W (hex "0x030E")
    val WM_QUERYNEWPALETTE                           = W (hex "0x030F")
    val WM_PALETTEISCHANGING                         = W (hex "0x0310")
    val WM_PALETTECHANGED                            = W (hex "0x0311")
    val WM_HOTKEY                                    = W (hex "0x0312")
    val WM_PENWINFIRST                               = W (hex "0x0380")
    val WM_PENWINLAST                                = W (hex "0x038F")
    val WM_USER                                      = W (hex "0x0400")
end


(*------------------------------------------------------------------------------
INDEX: HitTest
------------------------------------------------------------------------------*)

(* WM_NCHITTEST system message *)

(*TYPE: HitTest *)
local
    datatype HitTest =
    W of int
in
    type HitTest = HitTest
    val HitTestE = encoding (W,fn W n => n)

    val HTERROR                                      = W ((~2))
    val HTTRANSPARENT                                = W ((~1))
    val HTNOWHERE                                    = W (0)
    val HTCLIENT                                     = W (1)
    val HTCAPTION                                    = W (2)
    val HTSYSMENU                                    = W (3)
    val HTGROWBOX                                    = W (4)
    val HTSIZE                                       = HTGROWBOX
    val HTMENU                                       = W (5)
    val HTHSCROLL                                    = W (6)
    val HTVSCROLL                                    = W (7)
    val HTMINBUTTON                                  = W (8)
    val HTMAXBUTTON                                  = W (9)
    val HTLEFT                                       = W (10)
    val HTRIGHT                                      = W (11)
    val HTTOP                                        = W (12)
    val HTTOPLEFT                                    = W (13)
    val HTTOPRIGHT                                   = W (14)
    val HTBOTTOM                                     = W (15)
    val HTBOTTOMLEFT                                 = W (16)
    val HTBOTTOMRIGHT                                = W (17)
    val HTBORDER                                     = W (18)
    val HTREDUCE                                     = HTMINBUTTON
    val HTZOOM                                       = HTMAXBUTTON
    val HTSIZEFIRST                                  = HTLEFT
    val HTSIZELAST                                   = HTBOTTOMRIGHT
end



(*------------------------------------------------------------------------------
INDEX: SendMessageTimeOut
------------------------------------------------------------------------------*)

(*TYPE: SendMessageTimeOut *)
local
    datatype SendMessageTimeOut =
    W of int
in
    type SendMessageTimeOut = SendMessageTimeOut
    val SendMessageTimeOutE = encoding (W,fn W n => n)

    val SMTO_NORMAL                                  = W (hex "0x0000")
    val SMTO_BLOCK                                   = W (hex "0x0001")
    val SMTO_ABORTIFHUNG                             = W (hex "0x0002")
end



(*------------------------------------------------------------------------------
INDEX: MessageActivateFlag
------------------------------------------------------------------------------*)

(* WM_MOUSEACTVATE message *)

(*TYPE: MessageActivateFlag *)
local
    datatype MessageActivateFlag =
    W of int
in
    type MessageActivateFlag = MessageActivateFlag
    val MessageActivateFlagE = encoding (W,fn W n => n)

    val MA_ACTIVATE                                  = W (1)
    val MA_ACTIVATEANDEAT                            = W (2)
    val MA_NOACTIVATE                                = W (3)
    val MA_NOACTIVATEANDEAT                          = W (4)
end



(*------------------------------------------------------------------------------
INDEX: SizeFlag
------------------------------------------------------------------------------*)

(* WM_SIZE message *)

(*TYPE: SizeFlag *)
local
    datatype SizeFlag =
    W of int
in
    type SizeFlag = SizeFlag
    val SizeFlagE = encoding (W,fn W n => n)

    val SIZE_RESTORED                                = W (0)
    val SIZE_MINIMIZED                               = W (1)
    val SIZE_MAXIMIZED                               = W (2)
    val SIZE_MAXSHOW                                 = W (3)
    val SIZE_MAXHIDE                                 = W (4)
    val SIZENORMAL                                   = SIZE_RESTORED
    val SIZEICONIC                                   = SIZE_MINIMIZED
    val SIZEFULLSCREEN                               = SIZE_MAXIMIZED
    val SIZEZOOMSHOW                                 = SIZE_MAXSHOW
    val SIZEZOOMHIDE                                 = SIZE_MAXHIDE
end



(*------------------------------------------------------------------------------
INDEX: WindowAttribute
------------------------------------------------------------------------------*)

(*TYPE: WindowAttribute *)
local
    datatype WindowAttribute =
    W of int
in
    type WindowAttribute = WindowAttribute
    val WindowAttributeE = encoding (W,fn W n => n)

    val WVR_ALIGNTOP                                 = W (hex "0x0010")
    val WVR_ALIGNLEFT                                = W (hex "0x0020")
    val WVR_ALIGNBOTTOM                              = W (hex "0x0040")
    val WVR_ALIGNRIGHT                               = W (hex "0x0080")
    val WVR_HREDRAW                                  = W (hex "0x0100")
    val WVR_VREDRAW                                  = W (hex "0x0200")
    val WVR_REDRAW                                   = orLE WindowAttributeE [WVR_HREDRAW,WVR_VREDRAW]
    val WVR_VALIDRECTS                               = W (hex "0x0400")
end



(*------------------------------------------------------------------------------
INDEX: MouseKey
------------------------------------------------------------------------------*)

(* Set if the left mouse button is down. See WM_LBUTTONDBLCLK, 
   WM_MBUTTONDBLCLK, WM_MBUTTONDOWN, WM_MBUTTONUP, 
   WM_MOUSEMOVE, WM_RBUTTONDBLCLK, WM_RBUTTONDOWN, 
   WM_RBUTTONUP. *)

(*TYPE: MouseKey *)
local
    datatype MouseKey =
    W of int
in
    type MouseKey = MouseKey
    val MouseKeyE = encoding (W,fn W n => n)

    val MK_LBUTTON                                   = W (hex "0x0001")
    val MK_RBUTTON                                   = W (hex "0x0002")
    val MK_SHIFT                                     = W (hex "0x0004")
    val MK_CONTROL                                   = W (hex "0x0008")
    val MK_MBUTTON                                   = W (hex "0x0010")
end



(*------------------------------------------------------------------------------
INDEX: WindowStyle
------------------------------------------------------------------------------*)

(* Window Styles *)
(* CreateMDIWindow()
   CreateWindow()  *)

(*TYPE: WindowStyle *)
local
    datatype WindowStyle =
    W of int
in
    type WindowStyle = WindowStyle
    val WindowStyleE = encoding (W,fn W n => n)

    val WS_OVERLAPPED                                = W (hex "0x00000000")
    val WS_POPUP                                     = W (hex "0x80000000")
    val WS_CHILD                                     = W (hex "0x40000000")
    val WS_MINIMIZE                                  = W (hex "0x20000000")
    val WS_VISIBLE                                   = W (hex "0x10000000")
    val WS_DISABLED                                  = W (hex "0x08000000")
    val WS_CLIPSIBLINGS                              = W (hex "0x04000000")
    val WS_CLIPCHILDREN                              = W (hex "0x02000000")
    val WS_MAXIMIZE                                  = W (hex "0x01000000")
    val WS_CAPTION                                   = W (hex "0x00C00000" (* WS_BORDER | WS_DLGFRAME *))
    val WS_BORDER                                    = W (hex "0x00800000")
    val WS_DLGFRAME                                  = W (hex "0x00400000")
    val WS_VSCROLL                                   = W (hex "0x00200000")
    val WS_HSCROLL                                   = W (hex "0x00100000")
    val WS_SYSMENU                                   = W (hex "0x00080000")
    val WS_THICKFRAME                                = W (hex "0x00040000")
    val WS_GROUP                                     = W (hex "0x00020000")
    val WS_TABSTOP                                   = W (hex "0x00010000")
    val WS_MINIMIZEBOX                               = W (hex "0x00020000")
    val WS_MAXIMIZEBOX                               = W (hex "0x00010000")
    val WS_TILED                                     = WS_OVERLAPPED
    val WS_ICONIC                                    = WS_MINIMIZE
    val WS_SIZEBOX                                   = WS_THICKFRAME
    val WS_OVERLAPPEDWINDOW                          = orLE WindowStyleE [WS_OVERLAPPED,
                                      WS_CAPTION,
                                      WS_SYSMENU,
                                      WS_THICKFRAME,
                                      WS_MINIMIZEBOX,
                                      WS_MAXIMIZEBOX]
    val WS_TILEDWINDOW                               = WS_OVERLAPPEDWINDOW
    val WS_POPUPWINDOW                               = orLE WindowStyleE [WS_POPUP,
                                      WS_BORDER,
                                      WS_SYSMENU]
    val WS_CHILDWINDOW                               = WS_CHILD
end



(*------------------------------------------------------------------------------
INDEX: WindowStyleExtended
------------------------------------------------------------------------------*)

(* CreateWindowEx() *)

(*TYPE: WindowStyleExtended *)
local
    datatype WindowStyleExtended =
    W of int
in
    type WindowStyleExtended = WindowStyleExtended
    val WindowStyleExtendedE = encoding (W,fn W n => n)

    val WS_EX_DLGMODALFRAME                          = W (hex "0x00000001")
    val WS_EX_NOPARENTNOTIFY                         = W (hex "0x00000004")
    val WS_EX_TOPMOST                                = W (hex "0x00000008")
    val WS_EX_ACCEPTFILES                            = W (hex "0x00000010")
    val WS_EX_TRANSPARENT                            = W (hex "0x00000020")
end



(*------------------------------------------------------------------------------
INDEX: ClassStyle
------------------------------------------------------------------------------*)

(* Windows Class Styles *)

(*TYPE: ClassStyle *)
local
    datatype ClassStyle =
    W of int
in
    type ClassStyle = ClassStyle
    val ClassStyleE = encoding (W,fn W n => n)

    val CS_VREDRAW                                   = W (hex "0x0001")
    val CS_HREDRAW                                   = W (hex "0x0002")
    val CS_KEYCVTWINDOW                              = W (hex "0x0004")
    val CS_DBLCLKS                                   = W (hex "0x0008")
    val CS_OWNDC                                     = W (hex "0x0020")
    val CS_CLASSDC                                   = W (hex "0x0040")
    val CS_PARENTDC                                  = W (hex "0x0080")
    val CS_NOKEYCVT                                  = W (hex "0x0100")
    val CS_NOCLOSE                                   = W (hex "0x0200")
    val CS_SAVEBITS                                  = W (hex "0x0800")
    val CS_BYTEALIGNCLIENT                           = W (hex "0x1000")
    val CS_BYTEALIGNWINDOW                           = W (hex "0x2000")
    val CS_GLOBALCLASS                               = W (hex "0x4000")
end



(*------------------------------------------------------------------------------
INDEX: ClipboardFormat
------------------------------------------------------------------------------*)

(* Clipboard Formats *)
(* SetClipboardData() *)

(*TYPE: ClipboardFormat *)
local
    datatype ClipboardFormat =
    W of int
in
    type ClipboardFormat = ClipboardFormat
    val ClipboardFormatE = encoding (W,fn W n => n)

    val CF_TEXT                                      = W (1)
    val CF_BITMAP                                    = W (2)
    val CF_METAFILEPICT                              = W (3)
    val CF_SYLK                                      = W (4)
    val CF_DIF                                       = W (5)
    val CF_TIFF                                      = W (6)
    val CF_OEMTEXT                                   = W (7)
    val CF_DIB                                       = W (8)
    val CF_PALETTE                                   = W (9)
    val CF_PENDATA                                   = W (10)
    val CF_RIFF                                      = W (11)
    val CF_WAVE                                      = W (12)
    val CF_UNICODETEXT                               = W (13)
    val CF_ENHMETAFILE                               = W (14)
    val CF_OWNERDISPLAY                              = W (hex "0x0080")
    val CF_DSPTEXT                                   = W (hex "0x0081")
    val CF_DSPBITMAP                                 = W (hex "0x0082")
    val CF_DSPMETAFILEPICT                           = W (hex "0x0083")
    val CF_DSPENHMETAFILE                            = W (hex "0x008E")
    val CF_PRIVATEFIRST                              = W (hex "0x0200")
    val CF_PRIVATELAST                               = W (hex "0x02FF")
    val CF_GDIOBJFIRST                               = W (hex "0x0300")
    val CF_GDIOBJLAST                                = W (hex "0x03FF")
end



(*------------------------------------------------------------------------------
INDEX: AccelaratorFunction
------------------------------------------------------------------------------*)

(* Structure ACCEL *)

(*TYPE: AccelaratorFunction *)
local
    datatype AccelaratorFunction =
    W of int
in
    type AccelaratorFunction = AccelaratorFunction
    val AccelaratorFunctionE = encoding (W,fn W n => n)

    val FVIRTKEY                                     = W (TRUE (* Assumed to be == TRUE *))
    val FNOINVERT                                    = W (hex "0x02")
    val FSHIFT                                       = W (hex "0x04")
    val FCONTROL                                     = W (hex "0x08")
    val FALT                                         = W (hex "0x10")
end



(*------------------------------------------------------------------------------
INDEX: WindowPlacementFlag
------------------------------------------------------------------------------*)

(* Structure WINDOWPLACEMENT *)

(*TYPE: WindowPlacementFlag *)
local
    datatype WindowPlacementFlag =
    W of int
in
    type WindowPlacementFlag = WindowPlacementFlag
    val WindowPlacementFlagE = encoding (W,fn W n => n)

    val WPF_SETMINPOSITION                           = W (hex "0x0001")
    val WPF_RESTORETOMAXIMIZED                       = W (hex "0x0002")
end



(*------------------------------------------------------------------------------
INDEX: OwnerDrawnType
------------------------------------------------------------------------------*)

(* Owner-drawn combo box 
   MEASUREITEMSTRUCT
   DRAWITEMSTRUCT  *) 

(*TYPE: OwnerDrawnType *)
local
    datatype OwnerDrawnType =
    W of int
in
    type OwnerDrawnType = OwnerDrawnType
    val OwnerDrawnTypeE = encoding (W,fn W n => n)

    val ODT_MENU                                     = W (1)
    val ODT_LISTBOX                                  = W (2)
    val ODT_COMBOBOX                                 = W (3)
    val ODT_BUTTON                                   = W (4)
end



(*------------------------------------------------------------------------------
INDEX: OwnerDrawnAction
------------------------------------------------------------------------------*)

(* Structure DRAWITEMSTRUCT *)

(*TYPE: OwnerDrawnAction *)
local
    datatype OwnerDrawnAction =
    W of int
in
    type OwnerDrawnAction = OwnerDrawnAction
    val OwnerDrawnActionE = encoding (W,fn W n => n)

    val ODA_DRAWENTIRE                               = W (hex "0x0001")
    val ODA_SELECT                                   = W (hex "0x0002")
    val ODA_FOCUS                                    = W (hex "0x0004")
    val ODS_SELECTED                                 = W (hex "0x0001")
    val ODS_GRAYED                                   = W (hex "0x0002")
    val ODS_DISABLED                                 = W (hex "0x0004")
    val ODS_CHECKED                                  = W (hex "0x0008")
    val ODS_FOCUS                                    = W (hex "0x0010")
end



(*------------------------------------------------------------------------------
INDEX: PeekMessageAction
------------------------------------------------------------------------------*)

(*TYPE: PeekMessageAction *)
local
    datatype PeekMessageAction =
    W of int
in
    type PeekMessageAction = PeekMessageAction
    val PeekMessageActionE = encoding (W,fn W n => n)

    val PM_NOREMOVE                                  = W (hex "0x0000")
    val PM_REMOVE                                    = W (hex "0x0001")
    val PM_NOYIELD                                   = W (hex "0x0002")
end



(*------------------------------------------------------------------------------
INDEX: HotKeyModifier
------------------------------------------------------------------------------*)

(* RegisterHotKey() *)

(*TYPE: HotKeyModifier *)
local
    datatype HotKeyModifier =
    W of int
in
    type HotKeyModifier = HotKeyModifier
    val HotKeyModifierE = encoding (W,fn W n => n)

    val MOD_ALT                                      = W (hex "0x0001")
    val MOD_CONTROL                                  = W (hex "0x0002")
    val MOD_SHIFT                                    = W (hex "0x0004")
end



(*------------------------------------------------------------------------------
INDEX: HotKeyIdentifier
------------------------------------------------------------------------------*)

(* WM_HOTKEY Message *)

(*TYPE: HotKeyIdentifier *)
local
    datatype HotKeyIdentifier =
    W of int
in
    type HotKeyIdentifier = HotKeyIdentifier
    val HotKeyIdentifierE = encoding (W,fn W n => n)

    val IDHOT_SNAPWINDOW                             = W ((~1) (* SHIFT~PRINTSCRN *))
    val IDHOT_SNAPDESKTOP                            = W ((~2) (* PRINTSCRN *))
end



(*------------------------------------------------------------------------------
INDEX: ExitWindowMode
------------------------------------------------------------------------------*)

(* ExitWindows() *)

(*TYPE: ExitWindowMode *)
local
    datatype ExitWindowMode =
    W of int
in
    type ExitWindowMode = ExitWindowMode
    val ExitWindowModeE = encoding (W,fn W n => n)

    val EWX_LOGOFF                                   = W (0)
    val EWX_SHUTDOWN                                 = W (1)
    val EWX_REBOOT                                   = W (2)
    val EWX_FORCE                                    = W (4)
end



(*------------------------------------------------------------------------------
INDEX: probvlem
------------------------------------------------------------------------------*)

val CW_USEDEFAULT                                = (hex "0x80000000")



(*------------------------------------------------------------------------------
INDEX: SetWindowPositionStyle
------------------------------------------------------------------------------*)

(* SetWindowPos()
   DeferWindowPos()  *)

(*TYPE: SetWindowPositionStyle *)
local
    datatype SetWindowPositionStyle =
    W of int
in
    type SetWindowPositionStyle = SetWindowPositionStyle
    val SetWindowPositionStyleE = encoding (W,fn W n => n)

    val SWP_NOSIZE                                   = W (hex "0x0001")
    val SWP_NOMOVE                                   = W (hex "0x0002")
    val SWP_NOZORDER                                 = W (hex "0x0004")
    val SWP_NOREDRAW                                 = W (hex "0x0008")
    val SWP_NOACTIVATE                               = W (hex "0x0010")
    val SWP_FRAMECHANGED                             = W (hex "0x0020" (* The frame changed: send WM_NCCALCSIZE *))
    val SWP_SHOWWINDOW                               = W (hex "0x0040")
    val SWP_HIDEWINDOW                               = W (hex "0x0080")
    val SWP_NOCOPYBITS                               = W (hex "0x0100")
    val SWP_NOOWNERZORDER                            = W (hex "0x0200" (* Don't do owner Z ordering *))
    val SWP_DRAWFRAME                                = SWP_FRAMECHANGED
    val SWP_NOREPOSITION                             = SWP_NOOWNERZORDER
end


(*------------------------------------------------------------------------------
INDEX: KeyEvent
------------------------------------------------------------------------------*)

(* Key event *)

(*TYPE: KeyEvent *)
local
    datatype KeyEvent =
    W of int
in
    type KeyEvent = KeyEvent
    val KeyEventE = encoding (W,fn W n => n)

    val KEYEVENTF_EXTENDEDKEY                        = W (hex "0x0001")
    val KEYEVENTF_KEYUP                              = W (hex "0x0002")
end



(*------------------------------------------------------------------------------
INDEX: MouseEvent
------------------------------------------------------------------------------*)

(* mouse_event() *)

(*TYPE: MouseEvent *)
local
    datatype MouseEvent =
    W of int
in
    type MouseEvent = MouseEvent
    val MouseEventE = encoding (W,fn W n => n)

    val MOUSEEVENTF_MOVE                             = W (hex "0x0001" (* mouse move *))
    val MOUSEEVENTF_LEFTDOWN                         = W (hex "0x0002" (* left button down *))
    val MOUSEEVENTF_LEFTUP                           = W (hex "0x0004" (* left button up *))
    val MOUSEEVENTF_RIGHTDOWN                        = W (hex "0x0008" (* right button down *))
    val MOUSEEVENTF_RIGHTUP                          = W (hex "0x0010" (* right button up *))
    val MOUSEEVENTF_MIDDLEDOWN                       = W (hex "0x0020" (* middle button down *))
    val MOUSEEVENTF_MIDDLEUP                         = W (hex "0x0040" (* middle button up *))
    val MOUSEEVENTF_ABSOLUTE                         = W (hex "0x8000" (* absolute move *))
end



(*------------------------------------------------------------------------------
INDEX: QueueStatus
------------------------------------------------------------------------------*)

(* GetQueueStatus() *) 

(*TYPE: QueueStatus *)
local
    datatype QueueStatus =
    W of int
in
    type QueueStatus = QueueStatus
    val QueueStatusE = encoding (W,fn W n => n)

    val QS_KEY                                       = W (hex "0x01")
    val QS_MOUSEMOVE                                 = W (hex "0x02")
    val QS_MOUSEBUTTON                               = W (hex "0x04")
    val QS_POSTMESSAGE                               = W (hex "0x08")
    val QS_TIMER                                     = W (hex "0x10")
    val QS_PAINT                                     = W (hex "0x20")
    val QS_SENDMESSAGE                               = W (hex "0x40")
    val QS_HOTKEY                                    = W (hex "0x80")
    val QS_MOUSE                                     = orLE QueueStatusE [QS_MOUSEMOVE,
                                      QS_MOUSEBUTTON]
    val QS_INPUT                                     = orLE QueueStatusE [QS_MOUSE,
                                      QS_KEY]
    val QS_ALLEVENTS                                 = orLE QueueStatusE [QS_INPUT,
                                      QS_POSTMESSAGE,
                                      QS_TIMER,
                                      QS_PAINT,
                                      QS_HOTKEY]
    val QS_ALLINPUT                                  = orLE QueueStatusE [QS_SENDMESSAGE,
                                      QS_PAINT,
                                      QS_TIMER,
                                      QS_POSTMESSAGE,
                                      QS_MOUSEBUTTON,
                                      QS_MOUSEMOVE,
                                      QS_HOTKEY,
                                      QS_KEY] 
end



(*------------------------------------------------------------------------------
INDEX: SystemMetrics
------------------------------------------------------------------------------*)

(* GetSystemMetrics() *)

(*TYPE: SystemMetrics *)
local
    datatype SystemMetrics =
    W of int
in
    type SystemMetrics = SystemMetrics
    val SystemMetricsE = encoding (W,fn W n => n)

    val SM_CXSCREEN                                  = W (0)
    val SM_CYSCREEN                                  = W (1)
    val SM_CXVSCROLL                                 = W (2)
    val SM_CYHSCROLL                                 = W (3)
    val SM_CYCAPTION                                 = W (4)
    val SM_CXBORDER                                  = W (5)
    val SM_CYBORDER                                  = W (6)
    val SM_CXDLGFRAME                                = W (7)
    val SM_CYDLGFRAME                                = W (8)
    val SM_CYVTHUMB                                  = W (9)
    val SM_CXHTHUMB                                  = W (10)
    val SM_CXICON                                    = W (11)
    val SM_CYICON                                    = W (12)
    val SM_CXCURSOR                                  = W (13)
    val SM_CYCURSOR                                  = W (14)
    val SM_CYMENU                                    = W (15)
    val SM_CXFULLSCREEN                              = W (16)
    val SM_CYFULLSCREEN                              = W (17)
    val SM_CYKANJIWINDOW                             = W (18)
    val SM_MOUSEPRESENT                              = W (19)
    val SM_CYVSCROLL                                 = W (20)
    val SM_CXHSCROLL                                 = W (21)
    val SM_DEBUG                                     = W (22)
    val SM_SWAPBUTTON                                = W (23)
    val SM_RESERVED1                                 = W (24)
    val SM_RESERVED2                                 = W (25)
    val SM_RESERVED3                                 = W (26)
    val SM_RESERVED4                                 = W (27)
    val SM_CXMIN                                     = W (28)
    val SM_CYMIN                                     = W (29)
    val SM_CXSIZE                                    = W (30)
    val SM_CYSIZE                                    = W (31)
    val SM_CXFRAME                                   = W (32)
    val SM_CYFRAME                                   = W (33)
    val SM_CXMINTRACK                                = W (34)
    val SM_CYMINTRACK                                = W (35)
    val SM_CXDOUBLECLK                               = W (36)
    val SM_CYDOUBLECLK                               = W (37)
    val SM_CXICONSPACING                             = W (38)
    val SM_CYICONSPACING                             = W (39)
    val SM_MENUDROPALIGNMENT                         = W (40)
    val SM_PENWINDOWS                                = W (41)
    val SM_DBCSENABLED                               = W (42)
    val SM_CMOUSEBUTTONS                             = W (43)
    val SM_CMETRICS                                  = W (44)
end



(*------------------------------------------------------------------------------
INDEX: TrackPopMenu
------------------------------------------------------------------------------*)

(* TrackPopUpMenu() *) 

(*TYPE: TrackPopMenu *)
local
    datatype TrackPopMenu =
    W of int
in
    type TrackPopMenu = TrackPopMenu
    val TrackPopMenuE = encoding (W,fn W n => n)

    val TPM_LEFTBUTTON                               = W (hex "0x0000")
    val TPM_RIGHTBUTTON                              = W (hex "0x0002")
    val TPM_LEFTALIGN                                = W (hex "0x0000")
    val TPM_CENTERALIGN                              = W (hex "0x0004")
    val TPM_RIGHTALIGN                               = W (hex "0x0008")
end



(*------------------------------------------------------------------------------
INDEX: DrawTextMode
------------------------------------------------------------------------------*)

(* DrawText() *)

(*TYPE: DrawTextMode *)
local
    datatype DrawTextMode =
    W of int
in
    type DrawTextMode = DrawTextMode
    val DrawTextModeE = encoding (W,fn W n => n)

    val DT_TOP                                       = W (hex "0x0000")
    val DT_LEFT                                      = W (hex "0x0000")
    val DT_CENTER                                    = W (hex "0x0001")
    val DT_RIGHT                                     = W (hex "0x0002")
    val DT_VCENTER                                   = W (hex "0x0004")
    val DT_BOTTOM                                    = W (hex "0x0008")
    val DT_WORDBREAK                                 = W (hex "0x0010")
    val DT_SINGLELINE                                = W (hex "0x0020")
    val DT_EXPANDTABS                                = W (hex "0x0040")
    val DT_TABSTOP                                   = W (hex "0x0080")
    val DT_NOCLIP                                    = W (hex "0x0100")
    val DT_EXTERNALLEADING                           = W (hex "0x0200")
    val DT_CALCRECT                                  = W (hex "0x0400")
    val DT_NOPREFIX                                  = W (hex "0x0800")
    val DT_INTERNAL                                  = W (hex "0x1000")
end



(*------------------------------------------------------------------------------
INDEX: DeviceContextFlag
------------------------------------------------------------------------------*)

(* Flags for GetDCEx() *)

(*TYPE: DeviceContextFlag *)
local
    datatype DeviceContextFlag =
    W of int
in
    type DeviceContextFlag = DeviceContextFlag
    val DeviceContextFlagE = encoding (W,fn W n => n)

    val DCX_WINDOW                                   = W (hex "0x00000001")
    val DCX_CACHE                                    = W (hex "0x00000002")
    val DCX_NORESETATTRS                             = W (hex "0x00000004")
    val DCX_CLIPCHILDREN                             = W (hex "0x00000008")
    val DCX_CLIPSIBLINGS                             = W (hex "0x00000010")
    val DCX_PARENTCLIP                               = W (hex "0x00000020")
    val DCX_EXCLUDERGN                               = W (hex "0x00000040")
    val DCX_INTERSECTRGN                             = W (hex "0x00000080")
    val DCX_EXCLUDEUPDATE                            = W (hex "0x00000100")
    val DCX_INTERSECTUPDATE                          = W (hex "0x00000200")
    val DCX_LOCKWINDOWUPDATE                         = W (hex "0x00000400")
    val DCX_NORECOMPUTE                              = W (hex "0x00100000")
    val DCX_VALIDATE                                 = W (hex "0x00200000")
end



(*------------------------------------------------------------------------------
INDEX: RedrawWindowFlag
------------------------------------------------------------------------------*)

(* RedrawWindow() *)

(*TYPE: RedrawWindowFlag *)
local
    datatype RedrawWindowFlag =
    W of int
in
    type RedrawWindowFlag = RedrawWindowFlag
    val RedrawWindowFlagE = encoding (W,fn W n => n)

    val RDW_INVALIDATE                               = W (hex "0x0001")
    val RDW_INTERNALPAINT                            = W (hex "0x0002")
    val RDW_ERASE                                    = W (hex "0x0004")
    val RDW_VALIDATE                                 = W (hex "0x0008")
    val RDW_NOINTERNALPAINT                          = W (hex "0x0010")
    val RDW_NOERASE                                  = W (hex "0x0020")
    val RDW_NOCHILDREN                               = W (hex "0x0040")
    val RDW_ALLCHILDREN                              = W (hex "0x0080")
    val RDW_UPDATENOW                                = W (hex "0x0100")
    val RDW_ERASENOW                                 = W (hex "0x0200")
    val RDW_FRAME                                    = W (hex "0x0400")
    val RDW_NOFRAME                                  = W (hex "0x0800")
end



(*------------------------------------------------------------------------------
INDEX: ScrollWindowFlag
------------------------------------------------------------------------------*)

(* ScrollWindow() *)

(*TYPE: ScrollWindowFlag *)
local
    datatype ScrollWindowFlag =
    W of int
in
    type ScrollWindowFlag = ScrollWindowFlag
    val ScrollWindowFlagE = encoding (W,fn W n => n)

    val SW_SCROLLCHILDREN                            = W (hex "0x0001" (* Scroll children within *lprcScroll. *))
    val SW_INVALIDATE                                = W (hex "0x0002" (* Invalidate after scrolling *))
    val SW_ERASE                                     = W (hex "0x0004" (* If SW_INVALIDATE, don't send WM_ERASEBACKGROUND *))
end



(*------------------------------------------------------------------------------
INDEX: EnableScrollBarFlag
------------------------------------------------------------------------------*)

(* EnableScrollBar() *)

(*TYPE: EnableScrollBarFlag *)
local
    datatype EnableScrollBarFlag =
    W of int
in
    type EnableScrollBarFlag = EnableScrollBarFlag
    val EnableScrollBarFlagE = encoding (W,fn W n => n)

    val ESB_ENABLE_BOTH                              = W (hex "0x0000")
    val ESB_DISABLE_BOTH                             = W (hex "0x0003")
    val ESB_DISABLE_LEFT                             = W (hex "0x0001")
    val ESB_DISABLE_RIGHT                            = W (hex "0x0002")
    val ESB_DISABLE_UP                               = W (hex "0x0001")
    val ESB_DISABLE_DOWN                             = W (hex "0x0002")
    val ESB_DISABLE_LTUP                             = ESB_DISABLE_LEFT
    val ESB_DISABLE_RTDN                             = ESB_DISABLE_RIGHT
end



(*------------------------------------------------------------------------------
INDEX: MessageBoxValue
------------------------------------------------------------------------------*)

(* MessageBox()
   MessageBeep() *)

(*TYPE: MessageBoxValue *)
local
    datatype MessageBoxValue =
    W of int
in
    type MessageBoxValue = MessageBoxValue
    val MessageBoxValueE = encoding (W,fn W n => n)

    val MB_OK                                        = W (hex "0x00000000")
    val MB_OKCANCEL                                  = W (hex "0x00000001")
    val MB_ABORTRETRYIGNORE                          = W (hex "0x00000002")
    val MB_YESNOCANCEL                               = W (hex "0x00000003")
    val MB_YESNO                                     = W (hex "0x00000004")
    val MB_RETRYCANCEL                               = W (hex "0x00000005")
    val MB_ICONHAND                                  = W (hex "0x00000010")
    val MB_ICONQUESTION                              = W (hex "0x00000020")
    val MB_ICONEXCLAMATION                           = W (hex "0x00000030")
    val MB_ICONASTERISK                              = W (hex "0x00000040")
    val MB_ICONINFORMATION                           = MB_ICONASTERISK
    val MB_ICONSTOP                                  = MB_ICONHAND
    val MB_DEFBUTTON1                                = W (hex "0x00000000")
    val MB_DEFBUTTON2                                = W (hex "0x00000100")
    val MB_DEFBUTTON3                                = W (hex "0x00000200")
    val MB_APPLMODAL                                 = W (hex "0x00000000")
    val MB_SYSTEMMODAL                               = W (hex "0x00001000")
    val MB_TASKMODAL                                 = W (hex "0x00002000")
    val MB_NOFOCUS                                   = W (hex "0x00008000")
    val MB_SETFOREGROUND                             = W (hex "0x00010000")
    val MB_DEFAULT_DESKTOP_ONLY                      = W (hex "0x00020000")
    val MB_TYPEMASK                                  = W (hex "0x0000000F")
    val MB_ICONMASK                                  = W (hex "0x000000F0")
    val MB_DEFMASK                                   = W (hex "0x00000F00")
    val MB_MODEMASK                                  = W (hex "0x00003000")
    val MB_MISCMASK                                  = W (hex "0x0000C000")
end



(*------------------------------------------------------------------------------
INDEX: ControlColor
------------------------------------------------------------------------------*)

(* Color types and constants *)

(*TYPE: ControlColor *)
local
    datatype ControlColor =
    W of int
in
    type ControlColor = ControlColor
    val ControlColorE = encoding (W,fn W n => n)

    val CTLCOLOR_MSGBOX                              = W (0)
    val CTLCOLOR_EDIT                                = W (1)
    val CTLCOLOR_LISTBOX                             = W (2)
    val CTLCOLOR_BTN                                 = W (3)
    val CTLCOLOR_DLG                                 = W (4)
    val CTLCOLOR_SCROLLBAR                           = W (5)
    val CTLCOLOR_STATIC                              = W (6)
    val CTLCOLOR_MAX                                 = W (8 (* three bits max *))
end



(*------------------------------------------------------------------------------
INDEX: ColorType
------------------------------------------------------------------------------*)

(* Color types and constants *)

(*TYPE: ColorType *)
local
    datatype ColorType =
    W of int
in
    type ColorType = ColorType
    val ColorTypeE = encoding (W,fn W n => n)

    val COLOR_SCROLLBAR                              = W (0)
    val COLOR_BACKGROUND                             = W (1)
    val COLOR_ACTIVECAPTION                          = W (2)
    val COLOR_INACTIVECAPTION                        = W (3)
    val COLOR_MENU                                   = W (4)
    val COLOR_WINDOW                                 = W (5)
    val COLOR_WINDOWFRAME                            = W (6)
    val COLOR_MENUTEXT                               = W (7)
    val COLOR_WINDOWTEXT                             = W (8)
    val COLOR_CAPTIONTEXT                            = W (9)
    val COLOR_ACTIVEBORDER                           = W (10)
    val COLOR_INACTIVEBORDER                         = W (11)
    val COLOR_APPWORKSPACE                           = W (12)
    val COLOR_HIGHLIGHT                              = W (13)
    val COLOR_HIGHLIGHTTEXT                          = W (14)
    val COLOR_BTNFACE                                = W (15)
    val COLOR_BTNSHADOW                              = W (16)
    val COLOR_GRAYTEXT                               = W (17)
    val COLOR_BTNTEXT                                = W (18)
    val COLOR_INACTIVECAPTIONTEXT                    = W (19)
    val COLOR_BTNHIGHLIGHT                           = W (20)
end



(*------------------------------------------------------------------------------
INDEX: GetWindowFlag
------------------------------------------------------------------------------*)

(* GetWindow() *) 

(*TYPE: GetWindowFlag *)
local
    datatype GetWindowFlag =
    W of int
in
    type GetWindowFlag = GetWindowFlag
    val GetWindowFlagE = encoding (W,fn W n => n)

    val GW_HWNDFIRST                                 = W (0)
    val GW_HWNDLAST                                  = W (1)
    val GW_HWNDNEXT                                  = W (2)
    val GW_HWNDPREV                                  = W (3)
    val GW_OWNER                                     = W (4)
    val GW_CHILD                                     = W (5)
    val GW_MAX                                       = W (5)
end



(*------------------------------------------------------------------------------
INDEX: MenuFlag
------------------------------------------------------------------------------*)

(* Menu Flags *)
(* AppendMenu()
   InsertMenu()
   ModifyMenu()
   ChangeMenu()
   GetMenuState()
 *)

(*TYPE: MenuFlag *)
local
    datatype MenuFlag =
    W of int
in
    type MenuFlag = MenuFlag
    val MenuFlagE = encoding (W,fn W n => n)

    val MF_INSERT                                    = W (hex "0x00000000")
    val MF_CHANGE                                    = W (hex "0x00000080")
    val MF_APPEND                                    = W (hex "0x00000100")
    val MF_DELETE                                    = W (hex "0x00000200")
    val MF_REMOVE                                    = W (hex "0x00001000")
    val MF_BYCOMMAND                                 = W (hex "0x00000000")
    val MF_BYPOSITION                                = W (hex "0x00000400")
    val MF_SEPARATOR                                 = W (hex "0x00000800")
    val MF_ENABLED                                   = W (hex "0x00000000")
    val MF_GRAYED                                    = W (hex "0x00000001")
    val MF_DISABLED                                  = W (hex "0x00000002")
    val MF_UNCHECKED                                 = W (hex "0x00000000")
    val MF_CHECKED                                   = W (hex "0x00000008")
    val MF_USECHECKBITMAPS                           = W (hex "0x00000200")
    val MF_STRING                                    = W (hex "0x00000000")
    val MF_BITMAP                                    = W (hex "0x00000004")
    val MF_OWNERDRAW                                 = W (hex "0x00000100")
    val MF_POPUP                                     = W (hex "0x00000010")
    val MF_MENUBARBREAK                              = W (hex "0x00000020")
    val MF_MENUBREAK                                 = W (hex "0x00000040")
    val MF_UNHILITE                                  = W (hex "0x00000000")
    val MF_HILITE                                    = W (hex "0x00000080")
    val MF_SYSMENU                                   = W (hex "0x00002000")
    val MF_HELP                                      = W (hex "0x00004000")
    val MF_MOUSESELECT                               = W (hex "0x00008000")
    val MF_END                                       = W (hex "0x00000080")
end



(*------------------------------------------------------------------------------
INDEX: SystemCommand
------------------------------------------------------------------------------*)

(* System menu *)

(*TYPE: SystemCommand *)
local
    datatype SystemCommand =
    W of int
in
    type SystemCommand = SystemCommand
    val SystemCommandE = encoding (W,fn W n => n)

    val SC_SIZE                                      = W (hex "0xF000")
    val SC_MOVE                                      = W (hex "0xF010")
    val SC_MINIMIZE                                  = W (hex "0xF020")
    val SC_MAXIMIZE                                  = W (hex "0xF030")
    val SC_NEXTWINDOW                                = W (hex "0xF040")
    val SC_PREVWINDOW                                = W (hex "0xF050")
    val SC_CLOSE                                     = W (hex "0xF060")
    val SC_VSCROLL                                   = W (hex "0xF070")
    val SC_HSCROLL                                   = W (hex "0xF080")
    val SC_MOUSEMENU                                 = W (hex "0xF090")
    val SC_KEYMENU                                   = W (hex "0xF100")
    val SC_ARRANGE                                   = W (hex "0xF110")
    val SC_RESTORE                                   = W (hex "0xF120")
    val SC_TASKLIST                                  = W (hex "0xF130")
    val SC_SCREENSAVE                                = W (hex "0xF140")
    val SC_HOTKEY                                    = W (hex "0xF150")
    val SC_ICON                                      = SC_MINIMIZE
    val SC_ZOOM                                      = SC_MAXIMIZE
end



(*------------------------------------------------------------------------------
INDEX: CursorIdentifier
------------------------------------------------------------------------------*)

(* LoadCursor() *)

(*TYPE: CursorIdentifier *)
local
    datatype CursorIdentifier =
    W of int
in
    type CursorIdentifier = CursorIdentifier
    val CursorIdentifierE = encoding (W,fn W n => n)

    val IDC_ARROW                                    = W (MAKEINTRESOURCE(32512))
    val IDC_IBEAM                                    = W (MAKEINTRESOURCE(32513))
    val IDC_WAIT                                     = W (MAKEINTRESOURCE(32514))
    val IDC_CROSS                                    = W (MAKEINTRESOURCE(32515))
    val IDC_UPARROW                                  = W (MAKEINTRESOURCE(32516))
    val IDC_SIZE                                     = W (MAKEINTRESOURCE(32640))
    val IDC_ICON                                     = W (MAKEINTRESOURCE(32641))
    val IDC_SIZENWSE                                 = W (MAKEINTRESOURCE(32642))
    val IDC_SIZENESW                                 = W (MAKEINTRESOURCE(32643))
    val IDC_SIZEWE                                   = W (MAKEINTRESOURCE(32644))
    val IDC_SIZENS                                   = W (MAKEINTRESOURCE(32645))
    val IDC_SIZEALL                                  = W (MAKEINTRESOURCE(32646) (* not in win3.1 *))
    val IDC_NO                                       = W (MAKEINTRESOURCE(32648) (* not in win3.1 *))
    val IDC_APPSTARTING                              = W (MAKEINTRESOURCE(32650) (* not in win3.1 *))
end



(*------------------------------------------------------------------------------
INDEX: BitmapOption
------------------------------------------------------------------------------*)

(* LoadBitmap() *)

(*TYPE: BitmapOption *)
local
    datatype BitmapOption =
    W of int
in
    type BitmapOption = BitmapOption
    val BitmapOptionE = encoding (W,fn W n => n)

    val OBM_CLOSE                                    = W (32754)
    val OBM_UPARROW                                  = W (32753)
    val OBM_DNARROW                                  = W (32752)
    val OBM_RGARROW                                  = W (32751)
    val OBM_LFARROW                                  = W (32750)
    val OBM_REDUCE                                   = W (32749)
    val OBM_ZOOM                                     = W (32748)
    val OBM_RESTORE                                  = W (32747)
    val OBM_REDUCED                                  = W (32746)
    val OBM_ZOOMD                                    = W (32745)
    val OBM_RESTORED                                 = W (32744)
    val OBM_UPARROWD                                 = W (32743)
    val OBM_DNARROWD                                 = W (32742)
    val OBM_RGARROWD                                 = W (32741)
    val OBM_LFARROWD                                 = W (32740)
    val OBM_MNARROW                                  = W (32739)
    val OBM_COMBO                                    = W (32738)
    val OBM_UPARROWI                                 = W (32737)
    val OBM_DNARROWI                                 = W (32736)
    val OBM_RGARROWI                                 = W (32735)
    val OBM_LFARROWI                                 = W (32734)
    val OBM_OLD_CLOSE                                = W (32767)
    val OBM_SIZE                                     = W (32766)
    val OBM_OLD_UPARROW                              = W (32765)
    val OBM_OLD_DNARROW                              = W (32764)
    val OBM_OLD_RGARROW                              = W (32763)
    val OBM_OLD_LFARROW                              = W (32762)
    val OBM_BTSIZE                                   = W (32761)
    val OBM_CHECK                                    = W (32760)
    val OBM_CHECKBOXES                               = W (32759)
    val OBM_BTNCORNERS                               = W (32758)
    val OBM_OLD_REDUCE                               = W (32757)
    val OBM_OLD_ZOOM                                 = W (32756)
    val OBM_OLD_RESTORE                              = W (32755)
end



(*------------------------------------------------------------------------------
INDEX: OCRFlag
------------------------------------------------------------------------------*)

(*TYPE: OCRFlag *)
local
    datatype OCRFlag =
    W of int
in
    type OCRFlag = OCRFlag
    val OCRFlagE = encoding (W,fn W n => n)

    val OCR_NORMAL                                   = W (32512)
    val OCR_IBEAM                                    = W (32513)
    val OCR_WAIT                                     = W (32514)
    val OCR_CROSS                                    = W (32515)
    val OCR_UP                                       = W (32516)
    val OCR_SIZE                                     = W (32640)
    val OCR_ICON                                     = W (32641)
    val OCR_SIZENWSE                                 = W (32642)
    val OCR_SIZENESW                                 = W (32643)
    val OCR_SIZEWE                                   = W (32644)
    val OCR_SIZENS                                   = W (32645)
    val OCR_SIZEALL                                  = W (32646)
    val OCR_ICOCUR                                   = W (32647)
    val OCR_NO                                       = W (32648 (* not in win3.1 *))
end



(*------------------------------------------------------------------------------
INDEX: OICFlag
------------------------------------------------------------------------------*)

(*TYPE: OICFlag *)
local
    datatype OICFlag =
    W of int
in
    type OICFlag = OICFlag
    val OICFlagE = encoding (W,fn W n => n)

    val OIC_SAMPLE                                   = W (32512)
    val OIC_HAND                                     = W (32513)
    val OIC_QUES                                     = W (32514)
    val OIC_BANG                                     = W (32515)
    val OIC_NOTE                                     = W (32516)
end


(*------------------------------------------------------------------------------
INDEX: IconIdentifier
------------------------------------------------------------------------------*)

(* LoadIcon() *)

(*TYPE: IconIdentifier *)
local
    datatype IconIdentifier =
    W of int
in
    type IconIdentifier = IconIdentifier
    val IconIdentifierE = encoding (W,fn W n => n)

    val IDI_APPLICATION                              = W (MAKEINTRESOURCE(32512))
    val IDI_HAND                                     = W (MAKEINTRESOURCE(32513))
    val IDI_QUESTION                                 = W (MAKEINTRESOURCE(32514))
    val IDI_EXCLAMATION                              = W (MAKEINTRESOURCE(32515))
    val IDI_ASTERISK                                 = W (MAKEINTRESOURCE(32516))
end



(*------------------------------------------------------------------------------
INDEX: MessageBoxIdentifier
------------------------------------------------------------------------------*)

(* MessageBox() *) 

(*TYPE: MessageBoxIdentifier *)
local
    datatype MessageBoxIdentifier =
    W of int
in
    type MessageBoxIdentifier = MessageBoxIdentifier
    val MessageBoxIdentifierE = encoding (W,fn W n => n)

    val IDOK                                         = W (1)
    val IDCANCEL                                     = W (2)
    val IDABORT                                      = W (3)
    val IDRETRY                                      = W (4)
    val IDIGNORE                                     = W (5)
    val IDYES                                        = W (6)
    val IDNO                                         = W (7)
end



(*------------------------------------------------------------------------------
INDEX: EditStyle
------------------------------------------------------------------------------*)

(* CreateWindow() *)

(*TYPE: EditStyle *)
local
    datatype EditStyle =
    W of int
in
    type EditStyle = EditStyle
    val EditStyleE = encoding (W,fn W n => n)

    val ES_LEFT                                      = W (hex "0x0000")
    val ES_CENTER                                    = W (hex "0x0001")
    val ES_RIGHT                                     = W (hex "0x0002")
    val ES_MULTILINE                                 = W (hex "0x0004")
    val ES_UPPERCASE                                 = W (hex "0x0008")
    val ES_LOWERCASE                                 = W (hex "0x0010")
    val ES_PASSWORD                                  = W (hex "0x0020")
    val ES_AUTOVSCROLL                               = W (hex "0x0040")
    val ES_AUTOHSCROLL                               = W (hex "0x0080")
    val ES_NOHIDESEL                                 = W (hex "0x0100")
    val ES_OEMCONVERT                                = W (hex "0x0400")
    val ES_READONLY                                  = W (hex "0x0800")
    val ES_WANTRETURN                                = W (hex "0x1000")

    val ES_OVERLAPPED                                = W (hex "0x00000000")
    val ES_POPUP                                     = W (hex "0x80000000")
    val ES_CHILD                                     = W (hex "0x40000000")
    val ES_MINIMIZE                                  = W (hex "0x20000000")
    val ES_VISIBLE                                   = W (hex "0x10000000")
    val ES_DISABLED                                  = W (hex "0x08000000")
    val ES_CLIPSIBLINGS                              = W (hex "0x04000000")
    val ES_CLIPCHILDREN                              = W (hex "0x02000000")
    val ES_MAXIMIZE                                  = W (hex "0x01000000")
    val ES_CAPTION                                   = W (hex "0x00C00000")
    val ES_BORDER                                    = W (hex "0x00800000")
    val ES_DLGFRAME                                  = W (hex "0x00400000")
    val ES_VSCROLL                                   = W (hex "0x00200000")
    val ES_HSCROLL                                   = W (hex "0x00100000")
    val ES_SYSMENU                                   = W (hex "0x00080000")
    val ES_THICKFRAME                                = W (hex "0x00040000")
    val ES_GROUP                                     = W (hex "0x00020000")
    val ES_TABSTOP                                   = W (hex "0x00010000")
    val ES_MINIMIZEBOX                               = W (hex "0x00020000")
    val ES_MAXIMIZEBOX                               = W (hex "0x00010000")
    val ES_TILED                                     = ES_OVERLAPPED
    val ES_ICONIC                                    = ES_MINIMIZE
    val ES_SIZEBOX                                   = ES_THICKFRAME
    val ES_OVERLAPPEDWINDOW                          = orLE EditStyleE [ES_OVERLAPPED,
                                      ES_CAPTION,
                                      ES_SYSMENU,
                                      ES_THICKFRAME,
                                      ES_MINIMIZEBOX,
                                      ES_MAXIMIZEBOX]
    val ES_TILEDWINDOW                               = ES_OVERLAPPEDWINDOW
    val ES_POPUPWINDOW                               = orLE EditStyleE [ES_POPUP,
                                      ES_BORDER,
                                      ES_SYSMENU]
    val ES_CHILDWINDOW                               = ES_CHILD

end



(*------------------------------------------------------------------------------
INDEX: ControlNotification
------------------------------------------------------------------------------*)

(* WM_COMMAND message *)

(*TYPE: ControlNotification *)
local
    datatype ControlNotification =
    W of int
in
    type ControlNotification = ControlNotification
    val  ControlNotificationE = encoding (W,fn W n => n)

    (* Edit control notifications *)
    val EN_SETFOCUS                                  = W (hex "0x0100")
    val EN_KILLFOCUS                                 = W (hex "0x0200")
    val EN_CHANGE                                    = W (hex "0x0300")
    val EN_UPDATE                                    = W (hex "0x0400")
    val EN_ERRSPACE                                  = W (hex "0x0500")
    val EN_MAXTEXT                                   = W (hex "0x0501")
    val EN_HSCROLL                                   = W (hex "0x0601")
    val EN_VSCROLL                                   = W (hex "0x0602")

    (* Button control notifications *)
    val BN_CLICKED                                   = W (0)
    val BN_PAINT                                     = W (1)
    val BN_HILITE                                    = W (2)
    val BN_UNHILITE                                  = W (3)
    val BN_DISABLE                                   = W (4)
    val BN_DOUBLECLICKED                             = W (5)

    (* ListBox control notifications *)
    val LBN_ERRSPACE                                 = W ((~2))
    val LBN_SELCHANGE                                = BN_PAINT
    val LBN_DBLCLK                                   = BN_HILITE
    val LBN_SELCANCEL                                = BN_UNHILITE
    val LBN_SETFOCUS                                 = BN_DISABLE
    val LBN_KILLFOCUS                                = BN_DOUBLECLICKED

    (* ComboBox control notifications *)
    val CBN_ERRSPACE                                 = W ((~1))
    val CBN_SELCHANGE                                = BN_PAINT
    val CBN_DBLCLK                                   = BN_HILITE
    val CBN_SETFOCUS                                 = BN_UNHILITE
    val CBN_KILLFOCUS                                = BN_DISABLE
    val CBN_EDITCHANGE                               = BN_DOUBLECLICKED
    val CBN_EDITUPDATE                               = W (6)
    val CBN_DROPDOWN                                 = W (7)
    val CBN_CLOSEUP                                  = W (8)
    val CBN_SELENDOK                                 = W (9)
    val CBN_SELENDCANCEL                             = W (10)
end



(*------------------------------------------------------------------------------
INDEX: EditCtlBehaviour
------------------------------------------------------------------------------*)

(*TYPE: EditCtlBehaviour *)
local
    datatype EditCtlBehaviour =
    W of int
in
    type EditCtlBehaviour = EditCtlBehaviour
    val EditCtlBehaviourE = encoding (W,fn W n => n)

    val EM_GETSEL                                    = W (hex "0x00B0")
    val EM_SETSEL                                    = W (hex "0x00B1")
    val EM_GETRECT                                   = W (hex "0x00B2")
    val EM_SETRECT                                   = W (hex "0x00B3")
    val EM_SETRECTNP                                 = W (hex "0x00B4")
    val EM_SCROLL                                    = W (hex "0x00B5")
    val EM_LINESCROLL                                = W (hex "0x00B6")
    val EM_SCROLLCARET                               = W (hex "0x00B7")
    val EM_GETMODIFY                                 = W (hex "0x00B8")
    val EM_SETMODIFY                                 = W (hex "0x00B9")
    val EM_GETLINECOUNT                              = W (hex "0x00BA")
    val EM_LINEINDEX                                 = W (hex "0x00BB")
    val EM_SETHANDLE                                 = W (hex "0x00BC")
    val EM_GETHANDLE                                 = W (hex "0x00BD")
    val EM_GETTHUMB                                  = W (hex "0x00BE")
    val EM_LINELENGTH                                = W (hex "0x00C1")
    val EM_REPLACESEL                                = W (hex "0x00C2")
    val EM_GETLINE                                   = W (hex "0x00C4")
    val EM_LIMITTEXT                                 = W (hex "0x00C5")
    val EM_CANUNDO                                   = W (hex "0x00C6")
    val EM_UNDO                                      = W (hex "0x00C7")
    val EM_FMTLINES                                  = W (hex "0x00C8")
    val EM_LINEFROMCHAR                              = W (hex "0x00C9")
    val EM_SETTABSTOPS                               = W (hex "0x00CB")
    val EM_SETPASSWORDCHAR                           = W (hex "0x00CC")
    val EM_EMPTYUNDOBUFFER                           = W (hex "0x00CD")
    val EM_GETFIRSTVISIBLELINE                       = W (hex "0x00CE")
    val EM_SETREADONLY                               = W (hex "0x00CF")
    val EM_SETWORDBREAKPROC                          = W (hex "0x00D0")
    val EM_GETWORDBREAKPROC                          = W (hex "0x00D1")
    val EM_GETPASSWORDCHAR                           = W (hex "0x00D2")
end



(*------------------------------------------------------------------------------
INDEX: WordBreakFlag
------------------------------------------------------------------------------*)

(*TYPE: WordBreakFlag *)
local
    datatype WordBreakFlag =
    W of int
in
    type WordBreakFlag = WordBreakFlag
    val WordBreakFlagE = encoding (W,fn W n => n)

    val WB_LEFT                                      = W (0)
    val WB_RIGHT                                     = W (1)
    val WB_ISDELIMITER                               = W (2)
end



(*------------------------------------------------------------------------------
INDEX: ButtonStyle
------------------------------------------------------------------------------*)

(* Brush and Button styles *)
(* CreateWindow()
   IsDlgButtonChecked()
   *)

(*TYPE: ButtonStyle *)
local
    datatype ButtonStyle =
    W of int
in
    type ButtonStyle = ButtonStyle
    val ButtonStyleE = encoding (W,fn W n => n)

    val BS_PUSHBUTTON                                = W (hex "0x00")
    val BS_DEFPUSHBUTTON                             = W (hex "0x01")
    val BS_CHECKBOX                                  = W (hex "0x02")
    val BS_AUTOCHECKBOX                              = W (hex "0x03")
    val BS_RADIOBUTTON                               = W (hex "0x04")
    val BS_3STATE                                    = W (hex "0x05")
    val BS_AUTO3STATE                                = W (hex "0x06")
    val BS_GROUPBOX                                  = W (hex "0x07")
    val BS_USERBUTTON                                = W (hex "0x08")
    val BS_AUTORADIOBUTTON                           = W (hex "0x09")
    val BS_OWNERDRAW                                 = W (hex "0x0B")
    val BS_LEFTTEXT                                  = W (hex "0x20")

    val BS_OVERLAPPED                                = W (hex "0x00000000")
    val BS_POPUP                                     = W (hex "0x80000000")
    val BS_CHILD                                     = W (hex "0x40000000")
    val BS_MINIMIZE                                  = W (hex "0x20000000")
    val BS_VISIBLE                                   = W (hex "0x10000000")
    val BS_DISABLED                                  = W (hex "0x08000000")
    val BS_CLIPSIBLINGS                              = W (hex "0x04000000")
    val BS_CLIPCHILDREN                              = W (hex "0x02000000")
    val BS_MAXIMIZE                                  = W (hex "0x01000000")
    val BS_CAPTION                                   = W (hex "0x00C00000")
    val BS_BORDER                                    = W (hex "0x00800000")
    val BS_DLGFRAME                                  = W (hex "0x00400000")
    val BS_VSCROLL                                   = W (hex "0x00200000")
    val BS_HSCROLL                                   = W (hex "0x00100000")
    val BS_SYSMENU                                   = W (hex "0x00080000")
    val BS_THICKFRAME                                = W (hex "0x00040000")
    val BS_GROUP                                     = W (hex "0x00020000")
    val BS_TABSTOP                                   = W (hex "0x00010000")
    val BS_MINIMIZEBOX                               = W (hex "0x00020000")
    val BS_MAXIMIZEBOX                               = W (hex "0x00010000")
    val BS_TILED                                     = BS_OVERLAPPED
    val BS_ICONIC                                    = BS_MINIMIZE
    val BS_SIZEBOX                                   = BS_THICKFRAME
    val BS_OVERLAPPEDWINDOW                          = orLE ButtonStyleE [BS_OVERLAPPED,
                                      BS_CAPTION,
                                      BS_SYSMENU,
                                      BS_THICKFRAME,
                                      BS_MINIMIZEBOX,
                                      BS_MAXIMIZEBOX]
    val BS_TILEDWINDOW                               = BS_OVERLAPPEDWINDOW
    val BS_POPUPWINDOW                               = orLE ButtonStyleE [BS_POPUP,
                                      BS_BORDER,
                                      BS_SYSMENU]
    val BS_CHILDWINDOW                               = BS_CHILD

end


(*------------------------------------------------------------------------------
INDEX: ButtonMessageEnquiry
------------------------------------------------------------------------------*)

(* Button control *)

(*TYPE: ButtonMessageEnquiry *)
local
    datatype ButtonMessageEnquiry =
    W of int
in
    type ButtonMessageEnquiry = ButtonMessageEnquiry
    val ButtonMessageEnquiryE = encoding (W,fn W n => n)

    val BM_GETCHECK                                  = W (hex "0x00F0")
    val BM_SETCHECK                                  = W (hex "0x00F1")
    val BM_GETSTATE                                  = W (hex "0x00F2")
    val BM_SETSTATE                                  = W (hex "0x00F3")
    val BM_SETSTYLE                                  = W (hex "0x00F4")
end



(*------------------------------------------------------------------------------
INDEX: StaticStyle
------------------------------------------------------------------------------*)

(* Static controls *)
(* CreateWindow() *)

(*TYPE: StaticStyle *)
local
    datatype StaticStyle =
    W of int
in
    type StaticStyle = StaticStyle
    val StaticStyleE = encoding (W,fn W n => n)

    val SS_LEFT                                      = W (hex "0x00")
    val SS_CENTER                                    = W (hex "0x01")
    val SS_RIGHT                                     = W (hex "0x02")
    val SS_ICON                                      = W (hex "0x03")
    val SS_BLACKRECT                                 = W (hex "0x04")
    val SS_GRAYRECT                                  = W (hex "0x05")
    val SS_WHITERECT                                 = W (hex "0x06")
    val SS_BLACKFRAME                                = W (hex "0x07")
    val SS_GRAYFRAME                                 = W (hex "0x08")
    val SS_WHITEFRAME                                = W (hex "0x09")
    val SS_USERITEM                                  = W (hex "0x0A")
    val SS_SIMPLE                                    = W (hex "0x0B")
    val SS_LEFTNOWORDWRAP                            = W (hex "0x0C")
    val SS_NOPREFIX                                  = W (hex "0x80" (* Don't do "&" character translation *))

    val SS_OVERLAPPED                                = W (hex "0x00000000")
    val SS_POPUP                                     = W (hex "0x80000000")
    val SS_CHILD                                     = W (hex "0x40000000")
    val SS_MINIMIZE                                  = W (hex "0x20000000")
    val SS_VISIBLE                                   = W (hex "0x10000000")
    val SS_DISABLED                                  = W (hex "0x08000000")
    val SS_CLIPSIBLINGS                              = W (hex "0x04000000")
    val SS_CLIPCHILDREN                              = W (hex "0x02000000")
    val SS_MAXIMIZE                                  = W (hex "0x01000000")
    val SS_CAPTION                                   = W (hex "0x00C00000")
    val SS_BORDER                                    = W (hex "0x00800000")
    val SS_DLGFRAME                                  = W (hex "0x00400000")
    val SS_VSCROLL                                   = W (hex "0x00200000")
    val SS_HSCROLL                                   = W (hex "0x00100000")
    val SS_SYSMENU                                   = W (hex "0x00080000")
    val SS_THICKFRAME                                = W (hex "0x00040000")
    val SS_GROUP                                     = W (hex "0x00020000")
    val SS_TABSTOP                                   = W (hex "0x00010000")
    val SS_MINIMIZEBOX                               = W (hex "0x00020000")
    val SS_MAXIMIZEBOX                               = W (hex "0x00010000")
    val SS_TILED                                     = SS_OVERLAPPED
    val SS_ICONIC                                    = SS_MINIMIZE
    val SS_SIZEBOX                                   = SS_THICKFRAME
    val SS_OVERLAPPEDWINDOW                          = orLE StaticStyleE [SS_OVERLAPPED,
                                      SS_CAPTION,
                                      SS_SYSMENU,
                                      SS_THICKFRAME,
                                      SS_MINIMIZEBOX,
                                      SS_MAXIMIZEBOX]
    val SS_TILEDWINDOW                               = SS_OVERLAPPEDWINDOW
    val SS_POPUPWINDOW                               = orLE StaticStyleE [SS_POPUP,
                                      SS_BORDER,
                                      SS_SYSMENU]
    val SS_CHILDWINDOW                               = SS_CHILD

end



(*------------------------------------------------------------------------------
INDEX: StaticMessage
------------------------------------------------------------------------------*)

(* Static control messages *)

(*TYPE: StaticMessage *)
local
    datatype StaticMessage =
    W of int
in
    type StaticMessage = StaticMessage
    val StaticMessageE = encoding (W,fn W n => n)

    val STM_SETICON                                  = W (hex "0x170")
    val STM_GETICON                                  = W (hex "0x171")
    val STM_MSGMAX                                   = W (hex "0x172")
end



(*------------------------------------------------------------------------------
INDEX: DWLParameter
------------------------------------------------------------------------------*)

(* SetWindowLong() *)

(*TYPE: DWLParameter *)
local
    datatype DWLParameter =
    W of int
in
    type DWLParameter = DWLParameter
    val DWLParameterE = encoding (W,fn W n => n)

    val DWL_MSGRESULT                                = W (0)
    val DWL_DLGPROC                                  = W (4)
    val DWL_USER                                     = W (8)
end



(*------------------------------------------------------------------------------
INDEX: DialogBoxFlag
------------------------------------------------------------------------------*)

(* Dialog Box flags *)

(*TYPE: DialogBoxFlag *)
local
    datatype DialogBoxFlag =
    W of int
in
    type DialogBoxFlag = DialogBoxFlag
    val DialogBoxFlagE = encoding (W,fn W n => n)

    val DDL_READWRITE                                = W (hex "0x0000")
    val DDL_READONLY                                 = W (hex "0x0001")
    val DDL_HIDDEN                                   = W (hex "0x0002")
    val DDL_SYSTEM                                   = W (hex "0x0004")
    val DDL_DIRECTORY                                = W (hex "0x0010")
    val DDL_ARCHIVE                                  = W (hex "0x0020")
    val DDL_POSTMSGS                                 = W (hex "0x2000")
    val DDL_DRIVES                                   = W (hex "0x4000")
    val DDL_EXCLUSIVE                                = W (hex "0x8000")
end



(*------------------------------------------------------------------------------
INDEX: DialogStyle
------------------------------------------------------------------------------*)

(* CreateWindow() *)

(*TYPE: DialogStyle *)
local
    datatype DialogStyle =
    W of int
in
    type DialogStyle = DialogStyle
    val DialogStyleE = encoding (W,fn W n => n)

    val DS_ABSALIGN                                  = W (hex "0x01")
    val DS_SYSMODAL                                  = W (hex "0x02")
    val DS_LOCALEDIT                                 = W (hex "0x20" (* Edit items get Local storage. *))
    val DS_SETFONT                                   = W (hex "0x40" (* User specified font for Dlg controls *))
    val DS_MODALFRAME                                = W (hex "0x80" (* Can be combined with WS_CAPTION *))
    val DS_NOIDLEMSG                                 = W (hex "0x100" (* WM_ENTERIDLE message will not be sent *))
    val DS_SETFOREGROUND                             = W (hex "0x200" (* not in win3.1 *))
end



(*------------------------------------------------------------------------------
INDEX: DefaultMessageAction
------------------------------------------------------------------------------*)

(*TYPE: DefaultMessageAction *)
local
    datatype DefaultMessageAction =
    W of int
in
    type DefaultMessageAction = DefaultMessageAction
    val DefaultMessageActionE = encoding (W,fn W n => n)

    val DM_GETDEFID                                  = W ((repE WindowMessageE WM_USER+0))
    val DM_SETDEFID                                  = W ((repE WindowMessageE WM_USER+1))
end



(*------------------------------------------------------------------------------
INDEX: PROB
------------------------------------------------------------------------------*)

    val DC_HASDEFID                                  = (hex "0x534B")



(*------------------------------------------------------------------------------
INDEX: DialogCode
------------------------------------------------------------------------------*)

(* Dialog codes *)

(*TYPE: DialogCode *)
local
    datatype DialogCode =
    W of int
in
    type DialogCode = DialogCode
    val DialogCodeE = encoding (W,fn W n => n)

    val DLGC_WANTARROWS                              = W (hex "0x0001" (* Control wants arrow keys *))
    val DLGC_WANTTAB                                 = W (hex "0x0002" (* Control wants tab keys *))
    val DLGC_WANTALLKEYS                             = W (hex "0x0004" (* Control wants all keys *))
    val DLGC_WANTMESSAGE                             = W (hex "0x0004" (* Pass message to control *))
    val DLGC_HASSETSEL                               = W (hex "0x0008" (* Understands EM_SETSEL message *))
    val DLGC_DEFPUSHBUTTON                           = W (hex "0x0010" (* Default pushbutton *))
    val DLGC_UNDEFPUSHBUTTON                         = W (hex "0x0020" (* Non~default pushbutton *))
    val DLGC_RADIOBUTTON                             = W (hex "0x0040" (* Radio button *))
    val DLGC_WANTCHARS                               = W (hex "0x0080" (* Want WM_CHAR messages *))
    val DLGC_STATIC                                  = W (hex "0x0100" (* Static item: don't include *))
    val DLGC_BUTTON                                  = W (hex "0x2000" (* Button item: can be checked *))
end



(*------------------------------------------------------------------------------
INDEX: ListBoxFlag
------------------------------------------------------------------------------*)

(*TYPE: ListBoxFlag *)
local
    datatype ListBoxFlag =
    W of int
in
    type ListBoxFlag = ListBoxFlag
    val ListBoxFlagE = encoding (W,fn W n => n)

    val LB_CTLCODE                                   = W (0)
    val LB_OKAY                                      = W (0)
    val LB_ERR                                       = W ((~1))
    val LB_ERRSPACE                                  = W ((~2))
end


(*------------------------------------------------------------------------------
INDEX: ListBoxAction
------------------------------------------------------------------------------*)

(* ListBox return consts and messages *)

(*TYPE: ListBoxAction *)
local
    datatype ListBoxAction =
    W of int
in
    type ListBoxAction = ListBoxAction
    val ListBoxActionE = encoding (W,fn W n => n)

    val LB_ADDSTRING                                 = W (hex "0x0180")
    val LB_INSERTSTRING                              = W (hex "0x0181")
    val LB_DELETESTRING                              = W (hex "0x0182")
    val LB_SELITEMRANGEEX                            = W (hex "0x0183")
    val LB_RESETCONTENT                              = W (hex "0x0184")
    val LB_SETSEL                                    = W (hex "0x0185")
    val LB_SETCURSEL                                 = W (hex "0x0186")
    val LB_GETSEL                                    = W (hex "0x0187")
    val LB_GETCURSEL                                 = W (hex "0x0188")
    val LB_GETTEXT                                   = W (hex "0x0189")
    val LB_GETTEXTLEN                                = W (hex "0x018A")
    val LB_GETCOUNT                                  = W (hex "0x018B")
    val LB_SELECTSTRING                              = W (hex "0x018C")
    val LB_DIR                                       = W (hex "0x018D")
    val LB_GETTOPINDEX                               = W (hex "0x018E")
    val LB_FINDSTRING                                = W (hex "0x018F")
    val LB_GETSELCOUNT                               = W (hex "0x0190")
    val LB_GETSELITEMS                               = W (hex "0x0191")
    val LB_SETTABSTOPS                               = W (hex "0x0192")
    val LB_GETHORIZONTALEXTENT                       = W (hex "0x0193")
    val LB_SETHORIZONTALEXTENT                       = W (hex "0x0194")
    val LB_SETCOLUMNWIDTH                            = W (hex "0x0195")
    val LB_ADDFILE                                   = W (hex "0x0196")
    val LB_SETTOPINDEX                               = W (hex "0x0197")
    val LB_GETITEMRECT                               = W (hex "0x0198")
    val LB_GETITEMDATA                               = W (hex "0x0199")
    val LB_SETITEMDATA                               = W (hex "0x019A")
    val LB_SELITEMRANGE                              = W (hex "0x019B")
    val LB_SETANCHORINDEX                            = W (hex "0x019C")
    val LB_GETANCHORINDEX                            = W (hex "0x019D")
    val LB_SETCARETINDEX                             = W (hex "0x019E")
    val LB_GETCARETINDEX                             = W (hex "0x019F")
    val LB_SETITEMHEIGHT                             = W (hex "0x01A0")
    val LB_GETITEMHEIGHT                             = W (hex "0x01A1")
    val LB_FINDSTRINGEXACT                           = W (hex "0x01A2")
    val LB_SETLOCALE                                 = W (hex "0x01A5")
    val LB_GETLOCALE                                 = W (hex "0x01A6")
    val LB_SETCOUNT                                  = W (hex "0x01A7")
    val LB_MSGMAX                                    = W (hex "0x01A8")
end



(*------------------------------------------------------------------------------
INDEX: ListBoxStyle
------------------------------------------------------------------------------*)

(* List Box Styles *)

(*TYPE: ListBoxStyle *)
local
    datatype ListBoxStyle =
    W of int
in
    type ListBoxStyle = ListBoxStyle
    val ListBoxStyleE = encoding (W,fn W n => n)

    val LBS_NOTIFY                                   = W (hex "0x0001")
    val LBS_SORT                                     = W (hex "0x0002")
    val LBS_NOREDRAW                                 = W (hex "0x0004")
    val LBS_MULTIPLESEL                              = W (hex "0x0008")
    val LBS_OWNERDRAWFIXED                           = W (hex "0x0010")
    val LBS_OWNERDRAWVARIABLE                        = W (hex "0x0020")
    val LBS_HASSTRINGS                               = W (hex "0x0040")
    val LBS_USETABSTOPS                              = W (hex "0x0080")
    val LBS_NOINTEGRALHEIGHT                         = W (hex "0x0100")
    val LBS_MULTICOLUMN                              = W (hex "0x0200")
    val LBS_WANTKEYBOARDINPUT                        = W (hex "0x0400")
    val LBS_EXTENDEDSEL                              = W (hex "0x0800")
    val LBS_DISABLENOSCROLL                          = W (hex "0x1000")
    val LBS_NODATA                                   = W (hex "0x2000")
    val LBS_STANDARD                                 = orLE ListBoxStyleE [LBS_NOTIFY,
                                       LBS_SORT,
                                       W (repE WindowStyleE WS_VSCROLL),
                                       W (repE WindowStyleE WS_BORDER)]

    val LBS_OVERLAPPED                                = W (hex "0x00000000")
    val LBS_POPUP                                     = W (hex "0x80000000")
    val LBS_CHILD                                     = W (hex "0x40000000")
    val LBS_MINIMIZE                                  = W (hex "0x20000000")
    val LBS_VISIBLE                                   = W (hex "0x10000000")
    val LBS_DISABLED                                  = W (hex "0x08000000")
    val LBS_CLIPSIBLINGS                              = W (hex "0x04000000")
    val LBS_CLIPCHILDREN                              = W (hex "0x02000000")
    val LBS_MAXIMIZE                                  = W (hex "0x01000000")
    val LBS_CAPTION                                   = W (hex "0x00C00000")
    val LBS_BORDER                                    = W (hex "0x00800000")
    val LBS_DLGFRAME                                  = W (hex "0x00400000")
    val LBS_VSCROLL                                   = W (hex "0x00200000")
    val LBS_HSCROLL                                   = W (hex "0x00100000")
    val LBS_SYSMENU                                   = W (hex "0x00080000")
    val LBS_THICKFRAME                                = W (hex "0x00040000")
    val LBS_GROUP                                     = W (hex "0x00020000")
    val LBS_TABSTOP                                   = W (hex "0x00010000")
    val LBS_MINIMIZEBOX                               = W (hex "0x00020000")
    val LBS_MAXIMIZEBOX                               = W (hex "0x00010000")
    val LBS_TILED                                     = LBS_OVERLAPPED
    val LBS_ICONIC                                    = LBS_MINIMIZE
    val LBS_SIZEBOX                                   = LBS_THICKFRAME
    val LBS_OVERLAPPEDWINDOW                          = orLE ListBoxStyleE [LBS_OVERLAPPED,
                                      LBS_CAPTION,
                                      LBS_SYSMENU,
                                      LBS_THICKFRAME,
                                      LBS_MINIMIZEBOX,
                                      LBS_MAXIMIZEBOX]
    val LBS_TILEDWINDOW                               = LBS_OVERLAPPEDWINDOW
    val LBS_POPUPWINDOW                               = orLE ListBoxStyleE [LBS_POPUP,
                                      LBS_BORDER,
                                      LBS_SYSMENU]
    val LBS_CHILDWINDOW                               = LBS_CHILD

end



(*------------------------------------------------------------------------------
INDEX: ComboBoxValue
------------------------------------------------------------------------------*)

(* Combo box message return value *)

(*TYPE: ComboBoxValue *)
local
    datatype ComboBoxValue =
    W of int
in
    type ComboBoxValue = ComboBoxValue
    val ComboBoxValueE = encoding (W,fn W n => n)

    val CB_OKAY                                      = W (0)
    val CB_ERR                                       = W ((~1))
    val CB_ERRSPACE                                  = W ((~2))
end


(*------------------------------------------------------------------------------
INDEX: ComboBoxStyle
------------------------------------------------------------------------------*)

(* Combo Box styles *)

(*TYPE: ComboBoxStyle *)
local
    datatype ComboBoxStyle =
    W of int
in
    type ComboBoxStyle = ComboBoxStyle
    val ComboBoxStyleE = encoding (W,fn W n => n)

    val CBS_SIMPLE                                   = W (hex "0x0001")
    val CBS_DROPDOWN                                 = W (hex "0x0002")
    val CBS_DROPDOWNLIST                             = W (hex "0x0003")
    val CBS_OWNERDRAWFIXED                           = W (hex "0x0010")
    val CBS_OWNERDRAWVARIABLE                        = W (hex "0x0020")
    val CBS_AUTOHSCROLL                              = W (hex "0x0040")
    val CBS_OEMCONVERT                               = W (hex "0x0080")
    val CBS_SORT                                     = W (hex "0x0100")
    val CBS_HASSTRINGS                               = W (hex "0x0200")
    val CBS_NOINTEGRALHEIGHT                         = W (hex "0x0400")
    val CBS_DISABLENOSCROLL                          = W (hex "0x0800")

    val CBS_OVERLAPPED                                = W (hex "0x00000000")
    val CBS_POPUP                                     = W (hex "0x80000000")
    val CBS_CHILD                                     = W (hex "0x40000000")
    val CBS_MINIMIZE                                  = W (hex "0x20000000")
    val CBS_VISIBLE                                   = W (hex "0x10000000")
    val CBS_DISABLED                                  = W (hex "0x08000000")
    val CBS_CLIPSIBLINGS                              = W (hex "0x04000000")
    val CBS_CLIPCHILDREN                              = W (hex "0x02000000")
    val CBS_MAXIMIZE                                  = W (hex "0x01000000")
    val CBS_CAPTION                                   = W (hex "0x00C00000")
    val CBS_BORDER                                    = W (hex "0x00800000")
    val CBS_DLGFRAME                                  = W (hex "0x00400000")
    val CBS_VSCROLL                                   = W (hex "0x00200000")
    val CBS_HSCROLL                                   = W (hex "0x00100000")
    val CBS_SYSMENU                                   = W (hex "0x00080000")
    val CBS_THICKFRAME                                = W (hex "0x00040000")
    val CBS_GROUP                                     = W (hex "0x00020000")
    val CBS_TABSTOP                                   = W (hex "0x00010000")
    val CBS_MINIMIZEBOX                               = W (hex "0x00020000")
    val CBS_MAXIMIZEBOX                               = W (hex "0x00010000")
    val CBS_TILED                                     = CBS_OVERLAPPED
    val CBS_ICONIC                                    = CBS_MINIMIZE
    val CBS_SIZEBOX                                   = CBS_THICKFRAME
    val CBS_OVERLAPPEDWINDOW                          = orLE ComboBoxStyleE [CBS_OVERLAPPED,
                                      CBS_CAPTION,
                                      CBS_SYSMENU,
                                      CBS_THICKFRAME,
                                      CBS_MINIMIZEBOX,
                                      CBS_MAXIMIZEBOX]
    val CBS_TILEDWINDOW                               = CBS_OVERLAPPEDWINDOW
    val CBS_POPUPWINDOW                               = orLE ComboBoxStyleE [CBS_POPUP,
                                      CBS_BORDER,
                                      CBS_SYSMENU]
    val CBS_CHILDWINDOW                               = CBS_CHILD

end



(*------------------------------------------------------------------------------
INDEX: ComboBoxAction
------------------------------------------------------------------------------*)

(* Combo Box return values and messages *)

(*TYPE: ComboBoxAction *)
local
    datatype ComboBoxAction =
    W of int
in
    type ComboBoxAction = ComboBoxAction
    val ComboBoxActionE = encoding (W,fn W n => n)

    val CB_GETEDITSEL                                = W (hex "0x0140")
    val CB_LIMITTEXT                                 = W (hex "0x0141")
    val CB_SETEDITSEL                                = W (hex "0x0142")
    val CB_ADDSTRING                                 = W (hex "0x0143")
    val CB_DELETESTRING                              = W (hex "0x0144")
    val CB_DIR                                       = W (hex "0x0145")
    val CB_GETCOUNT                                  = W (hex "0x0146")
    val CB_GETCURSEL                                 = W (hex "0x0147")
    val CB_GETLBTEXT                                 = W (hex "0x0148")
    val CB_GETLBTEXTLEN                              = W (hex "0x0149")
    val CB_INSERTSTRING                              = W (hex "0x014A")
    val CB_RESETCONTENT                              = W (hex "0x014B")
    val CB_FINDSTRING                                = W (hex "0x014C")
    val CB_SELECTSTRING                              = W (hex "0x014D")
    val CB_SETCURSEL                                 = W (hex "0x014E")
    val CB_SHOWDROPDOWN                              = W (hex "0x014F")
    val CB_GETITEMDATA                               = W (hex "0x0150")
    val CB_SETITEMDATA                               = W (hex "0x0151")
    val CB_GETDROPPEDCONTROLRECT                     = W (hex "0x0152")
    val CB_SETITEMHEIGHT                             = W (hex "0x0153")
    val CB_GETITEMHEIGHT                             = W (hex "0x0154")
    val CB_SETEXTENDEDUI                             = W (hex "0x0155")
    val CB_GETEXTENDEDUI                             = W (hex "0x0156")
    val CB_GETDROPPEDSTATE                           = W (hex "0x0157")
    val CB_FINDSTRINGEXACT                           = W (hex "0x0158")
    val CB_SETLOCALE                                 = W (hex "0x0159")
    val CB_GETLOCALE                                 = W (hex "0x015a")
    val CB_MSGMAX                                    = W (hex "0x015b")
end



(*------------------------------------------------------------------------------
INDEX: ScrollBarStyle
------------------------------------------------------------------------------*)

(* Scroll Bar styles *)

(*TYPE: ScrollBarStyle *)
local
    datatype ScrollBarStyle =
    W of int
in
    type ScrollBarStyle = ScrollBarStyle
    val ScrollBarStyleE = encoding (W,fn W n => n)

    val SBS_HORZ                                     = W (hex "0x0000")
    val SBS_VERT                                     = W (hex "0x0001")
    val SBS_TOPALIGN                                 = W (hex "0x0002")
    val SBS_LEFTALIGN                                = W (hex "0x0002")
    val SBS_BOTTOMALIGN                              = W (hex "0x0004")
    val SBS_RIGHTALIGN                               = W (hex "0x0004")
    val SBS_SIZEBOXTOPLEFTALIGN                      = W (hex "0x0002")
    val SBS_SIZEBOXBOTTOMRIGHTALIGN                  = W (hex "0x0004")
    val SBS_SIZEBOX                                  = W (hex "0x0008")

    val SBS_OVERLAPPED                                = W (hex "0x00000000")
    val SBS_POPUP                                     = W (hex "0x80000000")
    val SBS_CHILD                                     = W (hex "0x40000000")
    val SBS_MINIMIZE                                  = W (hex "0x20000000")
    val SBS_VISIBLE                                   = W (hex "0x10000000")
    val SBS_DISABLED                                  = W (hex "0x08000000")
    val SBS_CLIPSIBLINGS                              = W (hex "0x04000000")
    val SBS_CLIPCHILDREN                              = W (hex "0x02000000")
    val SBS_MAXIMIZE                                  = W (hex "0x01000000")
    val SBS_CAPTION                                   = W (hex "0x00C00000")
    val SBS_BORDER                                    = W (hex "0x00800000")
    val SBS_DLGFRAME                                  = W (hex "0x00400000")
    val SBS_VSCROLL                                   = W (hex "0x00200000")
    val SBS_HSCROLL                                   = W (hex "0x00100000")
    val SBS_SYSMENU                                   = W (hex "0x00080000")
    val SBS_THICKFRAME                                = W (hex "0x00040000")
    val SBS_GROUP                                     = W (hex "0x00020000")
    val SBS_TABSTOP                                   = W (hex "0x00010000")
    val SBS_MINIMIZEBOX                               = W (hex "0x00020000")
    val SBS_MAXIMIZEBOX                               = W (hex "0x00010000")
    val SBS_TILED                                     = SBS_OVERLAPPED
    val SBS_ICONIC                                    = SBS_MINIMIZE
    val SBS_SIZEBOX                                   = SBS_THICKFRAME
    val SBS_OVERLAPPEDWINDOW                          = orLE ScrollBarStyleE [SBS_OVERLAPPED,
                                      SBS_CAPTION,
                                      SBS_SYSMENU,
                                      SBS_THICKFRAME,
                                      SBS_MINIMIZEBOX,
                                      SBS_MAXIMIZEBOX]
    val SBS_TILEDWINDOW                               = SBS_OVERLAPPEDWINDOW
    val SBS_POPUPWINDOW                               = orLE ScrollBarStyleE [SBS_POPUP,
                                      SBS_BORDER,
                                      SBS_SYSMENU]
    val SBS_CHILDWINDOW                               = SBS_CHILD

end



(*------------------------------------------------------------------------------
INDEX: ScrollBarAction
------------------------------------------------------------------------------*)

(*TYPE: ScrollBarAction *)
local
    datatype ScrollBarAction =
    W of int
in
    type ScrollBarAction = ScrollBarAction
    val ScrollBarActionE = encoding (W,fn W n => n)

    val SBM_SETPOS                                   = W (hex "0x00E0" (* not in win3.1 *))
    val SBM_GETPOS                                   = W (hex "0x00E1" (* not in win3.1 *))
    val SBM_SETRANGE                                 = W (hex "0x00E2" (* not in win3.1 *))
    val SBM_SETRANGEREDRAW                           = W (hex "0x00E6" (* not in win3.1 *))
    val SBM_GETRANGE                                 = W (hex "0x00E3" (* not in win3.1 *))
    val SBM_ENABLE_ARROWS                            = W (hex "0x00E4" (* not in win3.1 *))
end


(*------------------------------------------------------------------------------
INDEX: HelpOption
------------------------------------------------------------------------------*)

(*TYPE: HelpOption *)
local
    datatype HelpOption =
    W of int
in
    type HelpOption = HelpOption
    val HelpOptionE = encoding (W,fn W n => n)

    val HELP_CONTEXT                                 = W (hex "0x0001" (* Display topic in ulTopic *))
    val HELP_QUIT                                    = W (hex "0x0002" (* Terminate help *))
    val HELP_INDEX                                   = W (hex "0x0003" (* Display index *))
    val HELP_CONTENTS                                = W (hex "0x0003")
    val HELP_HELPONHELP                              = W (hex "0x0004" (* Display help on using help *))
    val HELP_SETINDEX                                = W (hex "0x0005" (* Set current Index for multi index help *))
    val HELP_SETCONTENTS                             = W (hex "0x0005")
    val HELP_CONTEXTPOPUP                            = W (hex "0x0008")
    val HELP_FORCEFILE                               = W (hex "0x0009")
    val HELP_KEY                                     = W (hex "0x0101" (* Display topic for keyword in offabData *))
    val HELP_COMMAND                                 = W (hex "0x0102")
    val HELP_PARTIALKEY                              = W (hex "0x0105")
    val HELP_MULTIKEY                                = W (hex "0x0201")
    val HELP_SETWINPOS                               = W (hex "0x0203")
end



(*------------------------------------------------------------------------------
INDEX: SysParamInfo
------------------------------------------------------------------------------*)

(* SystemParameterInfo() *)

(*TYPE: SysParamInfo *)
local
    datatype SysParamInfo =
    W of int
in
    type SysParamInfo = SysParamInfo
    val SysParamInfoE = encoding (W,fn W n => n)

    val SPI_GETBEEP                                  = W (1)
    val SPI_SETBEEP                                  = W (2)
    val SPI_GETMOUSE                                 = W (3)
    val SPI_SETMOUSE                                 = W (4)
    val SPI_GETBORDER                                = W (5)
    val SPI_SETBORDER                                = W (6)
    val SPI_GETKEYBOARDSPEED                         = W (10)
    val SPI_SETKEYBOARDSPEED                         = W (11)
    val SPI_LANGDRIVER                               = W (12)
    val SPI_ICONHORIZONTALSPACING                    = W (13)
    val SPI_GETSCREENSAVETIMEOUT                     = W (14)
    val SPI_SETSCREENSAVETIMEOUT                     = W (15)
    val SPI_GETSCREENSAVEACTIVE                      = W (16)
    val SPI_SETSCREENSAVEACTIVE                      = W (17)
    val SPI_GETGRIDGRANULARITY                       = W (18)
    val SPI_SETGRIDGRANULARITY                       = W (19)
    val SPI_SETDESKWALLPAPER                         = W (20)
    val SPI_SETDESKPATTERN                           = W (21)
    val SPI_GETKEYBOARDDELAY                         = W (22)
    val SPI_SETKEYBOARDDELAY                         = W (23)
    val SPI_ICONVERTICALSPACING                      = W (24)
    val SPI_GETICONTITLEWRAP                         = W (25)
    val SPI_SETICONTITLEWRAP                         = W (26)
    val SPI_GETMENUDROPALIGNMENT                     = W (27)
    val SPI_SETMENUDROPALIGNMENT                     = W (28)
    val SPI_SETDOUBLECLKWIDTH                        = W (29)
    val SPI_SETDOUBLECLKHEIGHT                       = W (30)
    val SPI_GETICONTITLELOGFONT                      = W (31)
    val SPI_SETDOUBLECLICKTIME                       = W (32)
    val SPI_SETMOUSEBUTTONSWAP                       = W (33)
    val SPI_SETICONTITLELOGFONT                      = W (34)
    val SPI_GETFASTTASKSWITCH                        = W (35)
    val SPI_SETFASTTASKSWITCH                        = W (36)
    val SPIF_UPDATEINIFILE                           = W (hex "0x0001")
    val SPIF_SENDWININICHANGE                        = W (hex "0x0002")
end



(*------------------------------------------------------------------------------
INDEX: conversions
------------------------------------------------------------------------------*)

structure Conversions = struct
    val DocumentMode               = conversionOfEncoding DocumentModeE
    val DeviceCapability           = conversionOfEncoding DeviceCapabilityE
    val RasterOperation            = conversionOfEncoding RasterOperationE
    val RasterOpCode               = conversionOfEncoding RasterOpCodeE
    val GdiError                   = conversionOfEncoding GdiErrorE
    val ResultRegion               = conversionOfEncoding ResultRegionE
    val RegionOperation            = conversionOfEncoding RegionOperationE
    val StretchMode                = conversionOfEncoding StretchModeE
    val PolyFillMode               = conversionOfEncoding PolyFillModeE
    val TextAlign                  = conversionOfEncoding TextAlignE
    val VirtualTextAlign           = conversionOfEncoding VirtualTextAlignE
    val ExtendedTextOut            = conversionOfEncoding ExtendedTextOutE
    val DCBound                    = conversionOfEncoding DCBoundE
    val MetaFunction               = conversionOfEncoding MetaFunctionE
    val GDIEscapes                 = conversionOfEncoding GDIEscapesE
    val SpoolerErrorCode           = conversionOfEncoding SpoolerErrorCodeE
    val EnumObject                 = conversionOfEncoding EnumObjectE
    val XFormType                      = conversionOfEncoding XFormTypeE
    val BitmapCompression          = conversionOfEncoding BitmapCompressionE
    val TextMetricPitchandFamily   = conversionOfEncoding TextMetricPitchandFamilyE
    val NewTextMetric              = conversionOfEncoding NewTextMetricE
    val LogicalFont                = conversionOfEncoding LogicalFontE
    val OutputPrecision            = conversionOfEncoding OutputPrecisionE
    val ClippingPrecision          = conversionOfEncoding ClippingPrecisionE
    val OutputQuality              = conversionOfEncoding OutputQualityE
    val FontPitch                  = conversionOfEncoding FontPitchE
    val CharacterSet               = conversionOfEncoding CharacterSetE
    val FontFamily                 = conversionOfEncoding FontFamilyE
    val FontWeight                 = conversionOfEncoding FontWeightE
    val CulturePANOSE              = conversionOfEncoding CulturePANOSEE
    val FamilyTypePANOSE           = conversionOfEncoding FamilyTypePANOSEE
    val IndexPANOSE                = conversionOfEncoding IndexPANOSEE
    val SerifStylePANOSE           = conversionOfEncoding SerifStylePANOSEE
    val WeightPANOSE               = conversionOfEncoding WeightPANOSEE
    val ProportionPANOSE           = conversionOfEncoding ProportionPANOSEE
    val ContrastPANOSE             = conversionOfEncoding ContrastPANOSEE
    val StrokeVariationPANOSE      = conversionOfEncoding StrokeVariationPANOSEE
    val ArmStylePANOSE             = conversionOfEncoding ArmStylePANOSEE
    val LetterFormPANOSE           = conversionOfEncoding LetterFormPANOSEE
    val MidlinePANOSE              = conversionOfEncoding MidlinePANOSEE
    val XHeightPANOSE              = conversionOfEncoding XHeightPANOSEE
    val ExtLogFont                 = conversionOfEncoding ExtLogFontE
    val FontType                   = conversionOfEncoding FontTypeE
    val PaletteEntryFlag           = conversionOfEncoding PaletteEntryFlagE
    val BkMode                     = conversionOfEncoding BkModeE
    val GraphicsMode               = conversionOfEncoding GraphicsModeE
    val PointType                  = conversionOfEncoding PointTypeE
    val MapMode                    = conversionOfEncoding MapModeE
    val TransMode                  = conversionOfEncoding TransModeE
    val StockObjectType            = conversionOfEncoding StockObjectTypeE
    val BrushStyle                 = conversionOfEncoding BrushStyleE
    val HatchStyle                 = conversionOfEncoding HatchStyleE
    val PenStyle                   = conversionOfEncoding PenStyleE
    val DeviceItem                 = conversionOfEncoding DeviceItemE
    val DeviceTechnology           = conversionOfEncoding DeviceTechnologyE
    val CurveCapability            = conversionOfEncoding CurveCapabilityE
    val LineCapability             = conversionOfEncoding LineCapabilityE
    val PolygonalCapability        = conversionOfEncoding PolygonalCapabilityE
    val ClippingCapability         = conversionOfEncoding ClippingCapabilityE
    val TextCapability             = conversionOfEncoding TextCapabilityE
    val RasterCapability           = conversionOfEncoding RasterCapabilityE
    val DeviceIndependentPalette   = conversionOfEncoding DeviceIndependentPaletteE
    val SystemPaletteUse           = conversionOfEncoding SystemPaletteUseE
    val BitmapInit                 = conversionOfEncoding BitmapInitE
    val FloodFillMode              = conversionOfEncoding FloodFillModeE
    val DeviceMode                 = conversionOfEncoding DeviceModeE
    val PaperOrientation           = conversionOfEncoding PaperOrientationE
    val PaperSize                  = conversionOfEncoding PaperSizeE
    val BinMode                    = conversionOfEncoding BinModeE
    val DevModeResolution          = conversionOfEncoding DevModeResolutionE
    val DocumentMode               = conversionOfEncoding DocumentModeE
    val DeviceCapability           = conversionOfEncoding DeviceCapabilityE
    val ResourceType               = conversionOfEncoding ResourceTypeE
    val ScrollBarType              = conversionOfEncoding ScrollBarTypeE
    val ShowWindowMode             = conversionOfEncoding ShowWindowModeE
    val KeyFunction                = conversionOfEncoding KeyFunctionE
    val VirtualKey                 = conversionOfEncoding VirtualKeyE
    val WindowHook                 = conversionOfEncoding WindowHookE
    val HookCode                   = conversionOfEncoding HookCodeE
    val HookProcedureCode          = conversionOfEncoding HookProcedureCodeE
    val InputEventType             = conversionOfEncoding InputEventTypeE
    val KeyLayoutFlag              = conversionOfEncoding KeyLayoutFlagE
    val DesktopOperation           = conversionOfEncoding DesktopOperationE
    val WindowStationAccess        = conversionOfEncoding WindowStationAccessE
    val GetWindowLongParam         = conversionOfEncoding GetWindowLongParamE
    val GetWindowLongValue         = conversionOfEncoding GetWindowLongValueE
    val WindowActivationState      = conversionOfEncoding WindowActivationStateE
    val PowerState                 = conversionOfEncoding PowerStateE
    val WindowMessage              = conversionOfEncoding WindowMessageE
    val HitTest                    = conversionOfEncoding HitTestE
    val SendMessageTimeOut         = conversionOfEncoding SendMessageTimeOutE
    val MessageActivateFlag        = conversionOfEncoding MessageActivateFlagE
    val SizeFlag                   = conversionOfEncoding SizeFlagE
    val WindowAttribute            = conversionOfEncoding WindowAttributeE
    val MouseKey                   = conversionOfEncoding MouseKeyE
    val WindowStyle                = conversionOfEncoding WindowStyleE
    val WindowStyleExtended        = conversionOfEncoding WindowStyleExtendedE
    val ClassStyle                 = conversionOfEncoding ClassStyleE
    val ClipboardFormat            = conversionOfEncoding ClipboardFormatE
    val AccelaratorFunction        = conversionOfEncoding AccelaratorFunctionE
    val WindowPlacementFlag        = conversionOfEncoding WindowPlacementFlagE
    val OwnerDrawnType             = conversionOfEncoding OwnerDrawnTypeE
    val OwnerDrawnAction           = conversionOfEncoding OwnerDrawnActionE
    val PeekMessageAction          = conversionOfEncoding PeekMessageActionE
    val HotKeyModifier             = conversionOfEncoding HotKeyModifierE
    val HotKeyIdentifier           = conversionOfEncoding HotKeyIdentifierE
    val ExitWindowMode             = conversionOfEncoding ExitWindowModeE
    val SetWindowPositionStyle     = conversionOfEncoding SetWindowPositionStyleE
    val KeyEvent                   = conversionOfEncoding KeyEventE
    val MouseEvent                 = conversionOfEncoding MouseEventE
    val QueueStatus                = conversionOfEncoding QueueStatusE
    val SystemMetrics              = conversionOfEncoding SystemMetricsE
    val TrackPopMenu               = conversionOfEncoding TrackPopMenuE
    val DrawTextMode               = conversionOfEncoding DrawTextModeE
    val DeviceContextFlag          = conversionOfEncoding DeviceContextFlagE
    val RedrawWindowFlag           = conversionOfEncoding RedrawWindowFlagE
    val ScrollWindowFlag           = conversionOfEncoding ScrollWindowFlagE
    val EnableScrollBarFlag        = conversionOfEncoding EnableScrollBarFlagE
    val MessageBoxValue            = conversionOfEncoding MessageBoxValueE
    val ControlColor               = conversionOfEncoding ControlColorE
    val ColorType                  = conversionOfEncoding ColorTypeE
    val GetWindowFlag              = conversionOfEncoding GetWindowFlagE
    val MenuFlag                   = conversionOfEncoding MenuFlagE
    val SystemCommand              = conversionOfEncoding SystemCommandE
    val CursorIdentifier           = conversionOfEncoding CursorIdentifierE
    val BitmapOption               = conversionOfEncoding BitmapOptionE
    val OCRFlag                    = conversionOfEncoding OCRFlagE
    val OICFlag                    = conversionOfEncoding OICFlagE
    val IconIdentifier             = conversionOfEncoding IconIdentifierE
    val MessageBoxIdentifier       = conversionOfEncoding MessageBoxIdentifierE
    val EditStyle                  = conversionOfEncoding EditStyleE
    val ControlNotification        = conversionOfEncoding ControlNotificationE
    val EditCtlBehaviour           = conversionOfEncoding EditCtlBehaviourE
    val WordBreakFlag              = conversionOfEncoding WordBreakFlagE
    val ButtonStyle                = conversionOfEncoding ButtonStyleE
    val ButtonMessageEnquiry       = conversionOfEncoding ButtonMessageEnquiryE
    val StaticStyle                = conversionOfEncoding StaticStyleE
    val StaticMessage              = conversionOfEncoding StaticMessageE
    val DWLParameter               = conversionOfEncoding DWLParameterE
    val DialogBoxFlag              = conversionOfEncoding DialogBoxFlagE
    val DialogStyle                = conversionOfEncoding DialogStyleE
    val DefaultMessageAction       = conversionOfEncoding DefaultMessageActionE
    val DialogCode                 = conversionOfEncoding DialogCodeE
    val ListBoxFlag                = conversionOfEncoding ListBoxFlagE
    val ListBoxAction              = conversionOfEncoding ListBoxActionE
    val ListBoxStyle               = conversionOfEncoding ListBoxStyleE
    val ComboBoxValue              = conversionOfEncoding ComboBoxValueE
    val ComboBoxStyle              = conversionOfEncoding ComboBoxStyleE
    val ComboBoxAction             = conversionOfEncoding ComboBoxActionE
    val ScrollBarStyle             = conversionOfEncoding ScrollBarStyleE
    val ScrollBarAction            = conversionOfEncoding ScrollBarActionE
    val HelpOption                 = conversionOfEncoding HelpOptionE
    val SysParamInfo               = conversionOfEncoding SysParamInfoE
end



(*------------------------------------------------------------------------------
INDEX: upper case conversions
------------------------------------------------------------------------------*)

structure UpperCaseConversions = struct
    val DOCUMENTMODE               = conversionOfEncoding DocumentModeE
    val DEVICECAPABILITY           = conversionOfEncoding DeviceCapabilityE
    val RASTEROPERATION            = conversionOfEncoding RasterOperationE
    val RASTEROPCODE               = conversionOfEncoding RasterOpCodeE
    val GDIERROR                   = conversionOfEncoding GdiErrorE
    val RESULTREGION               = conversionOfEncoding ResultRegionE
    val REGIONOPERATION            = conversionOfEncoding RegionOperationE
    val STRETCHMODE                = conversionOfEncoding StretchModeE
    val POLYFILLMODE               = conversionOfEncoding PolyFillModeE
    val TEXTALIGN                  = conversionOfEncoding TextAlignE
    val VIRTUALTEXTALIGN           = conversionOfEncoding VirtualTextAlignE
    val EXTENDEDTEXTOUT            = conversionOfEncoding ExtendedTextOutE
    val DCBOUND                    = conversionOfEncoding DCBoundE
    val METAFUNCTION               = conversionOfEncoding MetaFunctionE
    val GDIESCAPES                 = conversionOfEncoding GDIEscapesE
    val SPOOLERERRORCODE           = conversionOfEncoding SpoolerErrorCodeE
    val ENUMOBJECT                 = conversionOfEncoding EnumObjectE
    val XFORMTYPE                  = conversionOfEncoding XFormTypeE
    val BITMAPCOMPRESSION          = conversionOfEncoding BitmapCompressionE
    val TEXTMETRICPITCHANDFAMILY   = conversionOfEncoding TextMetricPitchandFamilyE
    val NEWTEXTMETRIC              = conversionOfEncoding NewTextMetricE
    val LOGICALFONT                = conversionOfEncoding LogicalFontE
    val OUTPUTPRECISION            = conversionOfEncoding OutputPrecisionE
    val CLIPPINGPRECISION          = conversionOfEncoding ClippingPrecisionE
    val OUTPUTQUALITY              = conversionOfEncoding OutputQualityE
    val FONTPITCH                  = conversionOfEncoding FontPitchE
    val CHARACTERSET               = conversionOfEncoding CharacterSetE
    val FONTFAMILY                 = conversionOfEncoding FontFamilyE
    val FONTWEIGHT                 = conversionOfEncoding FontWeightE
    val CULTUREPANOSE              = conversionOfEncoding CulturePANOSEE
    val FAMILYTYPEPANOSE           = conversionOfEncoding FamilyTypePANOSEE
    val INDEXPANOSE                = conversionOfEncoding IndexPANOSEE
    val SERIFSTYLEPANOSE           = conversionOfEncoding SerifStylePANOSEE
    val WEIGHTPANOSE               = conversionOfEncoding WeightPANOSEE
    val PROPORTIONPANOSE           = conversionOfEncoding ProportionPANOSEE
    val CONTRASTPANOSE             = conversionOfEncoding ContrastPANOSEE
    val STROKEVARIATIONPANOSE      = conversionOfEncoding StrokeVariationPANOSEE
    val ARMSTYLEPANOSE             = conversionOfEncoding ArmStylePANOSEE
    val LETTERFORMPANOSE           = conversionOfEncoding LetterFormPANOSEE
    val MIDLINEPANOSE              = conversionOfEncoding MidlinePANOSEE
    val XHEIGHTPANOSE              = conversionOfEncoding XHeightPANOSEE
    val EXTLOGFONT                 = conversionOfEncoding ExtLogFontE
    val FONTTYPE                   = conversionOfEncoding FontTypeE
    val PALETTEENTRYFLAG           = conversionOfEncoding PaletteEntryFlagE
    val BKMODE                     = conversionOfEncoding BkModeE
    val GRAPHICSMODE               = conversionOfEncoding GraphicsModeE
    val POINTTYPE                  = conversionOfEncoding PointTypeE
    val MAPMODE                    = conversionOfEncoding MapModeE
    val TRANSMODE                  = conversionOfEncoding TransModeE
    val STOCKOBJECTTYPE            = conversionOfEncoding StockObjectTypeE
    val BRUSHSTYLE                 = conversionOfEncoding BrushStyleE
    val HATCHSTYLE                 = conversionOfEncoding HatchStyleE
    val PENSTYLE                   = conversionOfEncoding PenStyleE
    val DEVICEITEM                 = conversionOfEncoding DeviceItemE
    val DEVICETECHNOLOGY           = conversionOfEncoding DeviceTechnologyE
    val CURVECAPABILITY            = conversionOfEncoding CurveCapabilityE
    val LINECAPABILITY             = conversionOfEncoding LineCapabilityE
    val POLYGONALCAPABILITY        = conversionOfEncoding PolygonalCapabilityE
    val CLIPPINGCAPABILITY         = conversionOfEncoding ClippingCapabilityE
    val TEXTCAPABILITY             = conversionOfEncoding TextCapabilityE
    val RASTERCAPABILITY           = conversionOfEncoding RasterCapabilityE
    val DEVICEINDEPENDENTPALETTE   = conversionOfEncoding DeviceIndependentPaletteE
    val SYSTEMPALETTEUSE           = conversionOfEncoding SystemPaletteUseE
    val BITMAPINIT                 = conversionOfEncoding BitmapInitE
    val FLOODFILLMODE              = conversionOfEncoding FloodFillModeE
    val DEVICEMODE                 = conversionOfEncoding DeviceModeE
    val PAPERORIENTATION           = conversionOfEncoding PaperOrientationE 
    val PAPERSIZE                  = conversionOfEncoding PaperSizeE
    val BINMODE                    = conversionOfEncoding BinModeE
    val DEVMODERESOLUTION          = conversionOfEncoding DevModeResolutionE
    val DOCUMENTMODE               = conversionOfEncoding DocumentModeE
    val DEVICECAPABILITY           = conversionOfEncoding DeviceCapabilityE
    val RESOURCETYPE               = conversionOfEncoding ResourceTypeE
    val SCROLLBARTYPE              = conversionOfEncoding ScrollBarTypeE
    val SHOWWINDOWMODE             = conversionOfEncoding ShowWindowModeE
    val KEYFUNCTION                = conversionOfEncoding KeyFunctionE
    val VIRTUALKEY                 = conversionOfEncoding VirtualKeyE
    val WINDOWHOOK                 = conversionOfEncoding WindowHookE
    val HOOKCODE                   = conversionOfEncoding HookCodeE
    val HOOKPROCEDURECODE          = conversionOfEncoding HookProcedureCodeE
    val INPUTEVENTTYPE             = conversionOfEncoding InputEventTypeE
    val KEYLAYOUTFLAG              = conversionOfEncoding KeyLayoutFlagE
    val DESKTOPOPERATION           = conversionOfEncoding DesktopOperationE
    val WINDOWSTATIONACCESS        = conversionOfEncoding WindowStationAccessE
    val GETWINDOWLONGPARAM         = conversionOfEncoding GetWindowLongParamE
    val GETWINDOWLONGVALUE         = conversionOfEncoding GetWindowLongValueE
    val WINDOWACTIVATIONSTATE      = conversionOfEncoding WindowActivationStateE
    val POWERSTATE                 = conversionOfEncoding PowerStateE
    val WINDOWMESSAGE              = conversionOfEncoding WindowMessageE
    val HITTEST                    = conversionOfEncoding HitTestE
    val SENDMESSAGETIMEOUT         = conversionOfEncoding SendMessageTimeOutE
    val MESSAGEACTIVATEFLAG        = conversionOfEncoding MessageActivateFlagE
    val SIZEFLAG                   = conversionOfEncoding SizeFlagE
    val WINDOWATTRIBUTE            = conversionOfEncoding WindowAttributeE
    val MOUSEKEY                   = conversionOfEncoding MouseKeyE
    val WINDOWSTYLE                = conversionOfEncoding WindowStyleE
    val WINDOWSTYLEEXTENDED        = conversionOfEncoding WindowStyleExtendedE
    val CLASSSTYLE                 = conversionOfEncoding ClassStyleE
    val CLIPBOARDFORMAT            = conversionOfEncoding ClipboardFormatE
    val ACCELARATORFUNCTION        = conversionOfEncoding AccelaratorFunctionE
    val WINDOWPLACEMENTFLAG        = conversionOfEncoding WindowPlacementFlagE
    val OWNERDRAWNTYPE             = conversionOfEncoding OwnerDrawnTypeE
    val OWNERDRAWNACTION           = conversionOfEncoding OwnerDrawnActionE
    val PEEKMESSAGEACTION          = conversionOfEncoding PeekMessageActionE
    val HOTKEYMODIFIER             = conversionOfEncoding HotKeyModifierE
    val HOTKEYIDENTIFIER           = conversionOfEncoding HotKeyIdentifierE
    val EXITWINDOWMODE             = conversionOfEncoding ExitWindowModeE
    val SETWINDOWPOSITIONSTYLE     = conversionOfEncoding SetWindowPositionStyleE
    val KEYEVENT                   = conversionOfEncoding KeyEventE
    val MOUSEEVENT                 = conversionOfEncoding MouseEventE
    val QUEUESTATUS                = conversionOfEncoding QueueStatusE
    val SYSTEMMETRICS              = conversionOfEncoding SystemMetricsE
    val TRACKPOPMENU               = conversionOfEncoding TrackPopMenuE
    val DRAWTEXTMODE               = conversionOfEncoding DrawTextModeE
    val DEVICECONTEXTFLAG          = conversionOfEncoding DeviceContextFlagE
    val REDRAWWINDOWFLAG           = conversionOfEncoding RedrawWindowFlagE
    val SCROLLWINDOWFLAG           = conversionOfEncoding ScrollWindowFlagE
    val ENABLESCROLLBARFLAG        = conversionOfEncoding EnableScrollBarFlagE
    val MESSAGEBOXVALUE            = conversionOfEncoding MessageBoxValueE
    val CONTROLCOLOR               = conversionOfEncoding ControlColorE
    val COLORTYPE                  = conversionOfEncoding ColorTypeE
    val GETWINDOWFLAG              = conversionOfEncoding GetWindowFlagE
    val MENUFLAG                   = conversionOfEncoding MenuFlagE
    val SYSTEMCOMMAND              = conversionOfEncoding SystemCommandE
    val CURSORIDENTIFIER           = conversionOfEncoding CursorIdentifierE
    val BITMAPOPTION               = conversionOfEncoding BitmapOptionE
    val OCRFLAG                    = conversionOfEncoding OCRFlagE
    val OICFLAG                    = conversionOfEncoding OICFlagE
    val ICONIDENTIFIER             = conversionOfEncoding IconIdentifierE
    val MESSAGEBOXIDENTIFIER       = conversionOfEncoding MessageBoxIdentifierE
    val EDITSTYLE                  = conversionOfEncoding EditStyleE
    val CONTROLNOTIFICATION        = conversionOfEncoding ControlNotificationE
    val EDITCTLBEHAVIOUR           = conversionOfEncoding EditCtlBehaviourE
    val WORDBREAKFLAG              = conversionOfEncoding WordBreakFlagE
    val BUTTONSTYLE                = conversionOfEncoding ButtonStyleE
    val BUTTONMESSAGEENQUIRY       = conversionOfEncoding ButtonMessageEnquiryE
    val STATICSTYLE                = conversionOfEncoding StaticStyleE
    val STATICMESSAGE              = conversionOfEncoding StaticMessageE
    val DWLPARAMETER               = conversionOfEncoding DWLParameterE
    val DIALOGBOXFLAG              = conversionOfEncoding DialogBoxFlagE
    val DIALOGSTYLE                = conversionOfEncoding DialogStyleE
    val DEFAULTMESSAGEACTION       = conversionOfEncoding DefaultMessageActionE
    val DIALOGCODE                 = conversionOfEncoding DialogCodeE
    val LISTBOXFLAG                = conversionOfEncoding ListBoxFlagE
    val LISTBOXACTION              = conversionOfEncoding ListBoxActionE
    val LISTBOXSTYLE               = conversionOfEncoding ListBoxStyleE
    val COMBOBOXVALUE              = conversionOfEncoding ComboBoxValueE
    val COMBOBOXSTYLE              = conversionOfEncoding ComboBoxStyleE
    val COMBOBOXACTION             = conversionOfEncoding ComboBoxActionE
    val SCROLLBARSTYLE             = conversionOfEncoding ScrollBarStyleE
    val SCROLLBARACTION            = conversionOfEncoding ScrollBarActionE
    val HELPOPTION                 = conversionOfEncoding HelpOptionE
    val SYSPARAMINFO               = conversionOfEncoding SysParamInfoE
    val ARCDIRECTION               = conversionOfEncoding ArcDirectionE
end



(*------------------------------------------------------------------------------
INDEX: ends
------------------------------------------------------------------------------*)

end

end;

