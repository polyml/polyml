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
(******************************************************************************
 ******************************************************************************
 ***                                                                        ***
 *** Copyright (c) Abstract Hardware Limited 1988 - 1994                    ***
 ***                                                                        ***
 *** Copyright (c) 1987 Digital Equipment Corporation                       ***
 *** Copyright (c) 1987 Massachusetts Institute of Technology               ***
 ***                                                                        ***
 ***                    All Rights Reserved                                 ***
 ***                                                                        ***
 *** Permission to use, copy, modify, and distribute this signature and     ***
 *** its documentation for any purpose and without fee is hereby granted,   ***
 *** provided that the above copyright notices appear in all copies and     ***
 *** that both the copyright notices and this permission notice appear in   ***
 *** supporting documentation, and that the names of Digital, MIT and AHL   ***
 *** not be used in advertising or publicity pertaining to distribution of  ***
 *** the signature without specific, written prior permission.  Digital,    ***
 *** MIT and AHL disclaim all warranties with regard to this signature,     ***
 *** including all implied warranties of merchantability and fitness, in    ***
 *** no event shall Digital, MIT or AHL be liable for any special,          ***
 *** indirect or consequential damages or any damages whatsoever resulting  ***
 *** from loss of use, data or profits, whether in an action of contract,   ***
 *** negligence or other tortious action, arising out of or in connection   ***
 *** with the use or performance of this signature.                         ***
 ***                                                                        ***
 ******************************************************************************
 ******************************************************************************)

signature XWINDOWS_SIG =
sig
  structure XAtoms   : XATOMS_SIG;
  structure XCursors : XCURSORS_SIG;
  structure XKeySyms : XKEYSYMS_SIG;

  exception XWindows of string ;
  
  include XEVENT_SIG ;
                     
  (* Geometry *)
  
  val Left:   XRectangle -> int ;
  val Right:  XRectangle -> int ;
  val Top:    XRectangle -> int ;
  val Bottom: XRectangle -> int ;
  val Width:  XRectangle -> int ; (* width and height are non-negative *)
  val Height: XRectangle -> int ;
  
  val TopLeft:     XRectangle -> XPoint ;
  val TopRight:    XRectangle -> XPoint ;
  val BottomLeft:  XRectangle -> XPoint ;
  val BottomRight: XRectangle -> XPoint ;
  
  exception XRectangle of { top:int,left:int,bottom:int,right:int } ;

  val Rect: { left:int,right:int,top:int,bottom:int } -> XRectangle ;
  val Area: { x:int,y:int,w:int,h:int }               -> XRectangle ;
  
  val DestructRect: XRectangle -> { left:int,right:int,top:int,bottom:int } ;
  val DestructArea: XRectangle -> { x:int,y:int,w:int,h:int } ;
  
  val SplitRect: XRectangle  -> (XPoint * XPoint) ;
  val MakeRect:  XPoint -> XPoint -> XRectangle ;
  
  val Reflect:       XRectangle -> XRectangle ;
  val NegativePoint: XPoint     -> XPoint ;
  
  datatype Section = Nothing | Section of XRectangle ;
  
  val Intersection: XRectangle -> XRectangle  -> Section ;
  val Union:        XRectangle -> XRectangle  -> XRectangle ;
  val OutsetRect:   int -> XRectangle -> XRectangle ;
  val OffsetRect:   XRectangle -> XPoint -> XRectangle ;
  val IncludePoint: XPoint -> XRectangle -> XRectangle ;
  
  (* meant to be used as infix operators *)
  
  val AddPoint:       (XPoint * XPoint) -> XPoint ;
  val SubtractPoint:  (XPoint * XPoint) -> XPoint ;
  
  (* meant to be used as infix predicates *)
  
  val Inside:               (XRectangle * XRectangle) -> bool ;
  val Overlap:              (XRectangle * XRectangle) -> bool ;
  val Within:               (XPoint     * XRectangle) -> bool ;
  val LeftOf:               (XPoint     * XRectangle) -> bool ;
  val RightOf:              (XPoint     * XRectangle) -> bool ;
  val AboveOf:              (XPoint     * XRectangle) -> bool ;
  val BelowOf:              (XPoint     * XRectangle) -> bool ;
  val HorizontallyAbutting: (XRectangle * XRectangle) -> bool ;
  val VerticallyAbutting:   (XRectangle * XRectangle) -> bool ;
  
  val origin: XPoint ;
  val empty:  XRectangle ;

  (* Colorcells *)
  
  datatype XColor = XColor of { doRed:   bool,
                                doGreen: bool,
                                doBlue:  bool,
                                red:     int,
                                green:   int,
                                blue:    int,
                                pixel:   int } ;
  
  val Pixel: XColor -> int ;
  val RGB:   XColor -> (int * int * int) ;
  
  val Not: int -> int ;       (* infix, for not-ing  pixel values *)
  val And: int * int -> int ; (* infix, for and-ing  pixel values *)
  val Or:  int * int -> int ; (* infix, for or-ing   pixel values *)
  val Xor: int * int -> int ; (* infix, for xor-ing  pixel values *)
  val >> : int * int -> int ; (* infix, for shifting pixel values *)
  val << : int * int -> int ; (* infix, for shifting pixel values *)
  
  val XAllocColor:       Colormap -> XColor -> XColor ;
  val XAllocColorCells:  Colormap -> bool -> int -> int -> (int list * int list) ;
  val XAllocColorPlanes: Colormap -> bool -> int -> int -> int -> int -> (int list * int * int * int) ;
  val XAllocNamedColor:  Colormap -> string -> (XColor * XColor) ;
  val XFreeColors:       Colormap -> int list -> int -> unit ;
  val XLookupColor:      Colormap -> string -> (XColor * XColor) ;
  val XParseColor:       Colormap -> string -> XColor ;
  val XQueryColor:       Colormap -> int -> XColor ;
  val XQueryColors:      Colormap -> int list -> XColor list ;
  val XStoreColor:       Colormap -> XColor -> unit ;
  val XStoreColors:      Colormap -> XColor list -> unit ;
  val XStoreNamedColor:  Colormap -> string -> int -> (bool * bool * bool) -> unit ;
  
  val BlackPixel: Display -> int ;
  val WhitePixel: Display -> int ;
  
  (* Colormaps *)
  
  datatype AllocType = AllocNone | AllocAll ;

  val XCopyColormapAndFree:    Colormap -> Colormap ;
  val XCreateColormap:         Drawable -> Visual -> AllocType -> Colormap ;
  val XFreeColormap:           Colormap -> unit ;
  val XInstallColormap:        Colormap -> unit ;
  val XListInstalledColormaps: Drawable -> Colormap list ;
  val XSetWindowColormap:      Drawable -> Colormap -> unit ;
  val XUninstallColormap:      Colormap -> unit ;
  
  val DefaultColormap: Display -> Colormap ;
  val DefaultVisual:   Display -> Visual ;
  val DisplayCells:    Display -> int ;
  
  datatype VisualClass = StaticGray | GrayScale | StaticColor | PseudoColor | TrueColor | DirectColor ;
  
  val VisualClass:     Visual -> VisualClass ;
  val VisualRedMask:   Visual -> int ;
  val VisualGreenMask: Visual -> int ;
  val VisualBlueMask:  Visual -> int ;
  
  (* Cursors *)
  
  val XCreateFontCursor:   Display -> int -> Cursor ;
  val XCreateGlyphCursor:  Font -> Font -> int -> int -> XColor -> XColor -> Cursor ;
  val XCreatePixmapCursor: Drawable -> Drawable -> XColor -> XColor -> XPoint -> Cursor ;
  val XDefineCursor:       Drawable -> Cursor -> unit ;
  val XFreeCursor:         Cursor   -> unit ;
  val XQueryBestCursor:    Drawable -> XRectangle -> XRectangle ;
  val XRecolorCursor:      Cursor -> XColor -> XColor -> unit ;
  val XUndefineCursor:     Drawable -> unit ;
  
  (* Display Specifications *)
  
  datatype ShapeClass = CursorShape | TileShape | StippleShape ;
  
  val XQueryBestSize: ShapeClass -> Drawable -> XRectangle -> XRectangle ;
      
  datatype BackingStore = NotUseful | WhenMapped | Always ;
  
  val XOpenDisplay: string -> Display ;

  val XStartDisplayHandler: Display -> unit ;

  val AllPlanes:         int ;
  val CellsOfScreen:     Display -> int ;
  val DefaultDepth:      Display -> int ;
  val DisplayHeight:     Display -> int ;
  val DisplayHeightMM:   Display -> int ;
  val DisplayPlanes:     Display -> int ;
  val DisplayString:     Display -> string ;
  val DisplayWidth:      Display -> int ;
  val DisplayWidthMM:    Display -> int ;
  val DoesBackingStore:  Display -> BackingStore ;
  val DoesSaveUnders:    Display -> bool ;
  val EventMaskOfScreen: Display -> EventMask list ;
  val MaxCmapsOfScreen:  Display -> int ;
  val MinCmapsOfScreen:  Display -> int ;
  val ProtocolRevision:  Display -> int ;
  val ProtocolVersion:   Display -> int ;
  val ServerVendor:      Display -> string ;
  val VendorRelease:     Display -> int ;
  
  (* Drawing Primitives *)
  
  datatype XArc = XArc of XRectangle * int * int ;
  
  datatype XTextItem   = XTextItem   of string   * int * Font ;
  datatype XTextItem16 = XTextItem16 of int list * int * Font ;
  
  datatype CoordMode = CoordModeOrigin | CoordModePrevious ;
  
  datatype PolyShape = Complex | Nonconvex | Convex ;
  
  val XClearArea:         Drawable -> XRectangle -> bool -> unit ;
  val XClearWindow:       Drawable -> unit ;
  val XCopyArea:          Drawable -> Drawable -> GC -> XPoint -> XRectangle -> unit ;
  val XCopyPlane:         Drawable -> Drawable -> GC -> XPoint -> XRectangle -> int -> unit ;
  val XDrawArc:           Drawable -> GC -> XArc -> unit ;
  val XDrawArcs:          Drawable -> GC -> XArc list -> unit ;
  val XDrawImageString:   Drawable -> GC -> XPoint -> string   -> unit ;
  val XDrawImageString16: Drawable -> GC -> XPoint -> int list -> unit ;
  val XDrawLine:          Drawable -> GC -> XPoint -> XPoint -> unit ;
  val XDrawLines:         Drawable -> GC -> XPoint list -> CoordMode -> unit ;
  val XDrawPoint:         Drawable -> GC -> XPoint -> unit ;
  val XDrawPoints:        Drawable -> GC -> XPoint list -> CoordMode -> unit ;
  val XDrawRectangle:     Drawable -> GC -> XRectangle -> unit ;
  val XDrawRectangles:    Drawable -> GC -> XRectangle list -> unit ;
  val XDrawSegments:      Drawable -> GC -> (XPoint * XPoint) list -> unit ;
  val XDrawString:        Drawable -> GC -> XPoint -> string   -> unit ;
  val XDrawString16:      Drawable -> GC -> XPoint -> int list -> unit ;
  val XDrawText:          Drawable -> GC -> XPoint -> XTextItem list -> unit ;
  val XDrawText16:        Drawable -> GC -> XPoint -> XTextItem16 list -> unit ;
  val XFillArc:           Drawable -> GC -> XArc -> unit ;
  val XFillArcs:          Drawable -> GC -> XArc list -> unit ;
  val XFillPolygon:       Drawable -> GC -> XPoint list -> PolyShape -> CoordMode -> unit ;
  val XFillRectangle:     Drawable -> GC -> XRectangle -> unit ;
  val XFillRectangles:    Drawable -> GC -> XRectangle list -> unit ;
  
  (* Events *)
  
  val XSelectInput: Drawable -> EventMask list -> unit ;
  
  val XSynchronize: Display -> int -> unit ;
  
  val XSetHandler: Drawable -> ('a XEvent * 'b -> 'b) -> 'b -> (int -> 'a -> unit) ;
  
  val NullHandler: 'a XEvent * 'b -> 'b ;
  
  val CurrentTime: int ;
  
  datatype RevertCode = RevertToParent | RevertToPointerRoot | RevertToNone ;
  
  val XSetInputFocus: Display -> Drawable -> RevertCode -> int -> unit ;
  val XGetInputFocus: Display -> (Drawable * RevertCode) ;
  
  val XSetSelectionOwner: Display -> int -> Drawable -> int -> unit ;
  val XGetSelectionOwner: Display -> int -> Drawable ;
  
  val XConvertSelection:    { selection:int, target:int, property:int, requestor:Drawable, time:int } -> unit ;
  val XSendSelectionNotify: { selection:int, target:int, property:int, requestor:Drawable, time:int } -> unit ;

  val XDeleteProperty: Drawable -> int -> unit ;

  val XInternAtom:  Display -> string -> bool -> int ;
  val XGetAtomName: Display -> int -> string ;
  
  (* Fonts *)
  
  datatype FontDirection = FontLeftToRight | FontRightToLeft ;
  
  datatype XCharStruct = XCharStruct of { lbearing:   int,
                                          rbearing:   int,
                                          width:      int,
                                          ascent:     int,
                                          descent:    int,
                                          attributes: int } ;
  
  datatype XFontStruct = XFontStruct of { font:          Font,
                                          direction:     FontDirection,
                                          minChar:       int,
                                          maxChar:       int,
                                          minByte1:      int,
                                          maxByte1:      int,
                                          allCharsExist: bool,
                                          defaultChar:   int,
                                          minBounds:     XCharStruct,
                                          maxBounds:     XCharStruct,
                                          perChar:       XCharStruct list,
                                          ascent:        int,
                                          descent:       int } ;
  
  val XFreeFont:          XFontStruct -> unit ;
  val XGetFontPath:       Display -> string list ;
  val XListFonts:         Display -> string -> int -> string list ;
  val XListFontsWithInfo: Display -> string -> int -> (string list * XFontStruct list) ;
  val XLoadFont:          Display -> string -> Font ;
  val XLoadQueryFont:     Display -> string -> XFontStruct ;
  val XQueryFont:         Font -> XFontStruct ;
  val XSetFontPath:       Display -> string list -> unit ;
  val XUnloadFont:        Font -> unit ;
  
  (* Graphics Context *)
  
  datatype GCArcMode = ArcChord | ArcPieSlice ;

  datatype GCOrder = Unsorted | YSorted | YXSorted | YXBanded ;

  datatype GCFillRule = EvenOddRule | WindingRule ;

  datatype GCFillStyle = FillSolid | FillTiled | FillStippled | FillOpaqueStippled ;

  datatype GCFunction = GXclear        | GXand        | GXandReverse | GXcopy
                      | GXandInverted  | GXnoop       | GXxor        | GXor
                      | GXnor          | GXequiv      | GXinvert     | GXorReverse
                      | GXcopyInverted | GXorInverted | GXnand       | GXset ;

  datatype GCLineStyle = LineSolid | LineOnOffDash | LineDoubleDash ;

  datatype GCCapStyle = CapNotLast | CapButt | CapRound | CapProjecting ;
  
  datatype GCJoinStyle = JoinMiter | JoinRound | JoinBevel ;
  
  datatype GCSubwindowMode = ClipByChildren | IncludeInferiors ;
  
  val DefaultGC: Display -> GC ;
  
  datatype XGCValue = GCFunction          of GCFunction
                    | GCPlaneMask         of int
                    | GCForeground        of int
                    | GCBackground        of int
                    | GCLineWidth         of int
                    | GCLineStyle         of GCLineStyle
                    | GCCapStyle          of GCCapStyle
                    | GCJoinStyle         of GCJoinStyle
                    | GCFillStyle         of GCFillStyle
                    | GCFillRule          of GCFillRule
                    | GCTile              of Drawable
                    | GCStipple           of Drawable
                    | GCTSOrigin          of XPoint
                    | GCFont              of Font
                    | GCSubwindowMode     of GCSubwindowMode
                    | GCGraphicsExposures of bool
                    | GCClipOrigin        of XPoint
                    | GCClipMask          of Drawable
                    | GCDashOffset        of int
                    | GCDashList          of int
                    | GCArcMode           of GCArcMode ;

  val XChangeGC:             GC -> XGCValue list -> unit ;
  val XCreateGC:             Drawable -> XGCValue list -> GC ;
  val XFreeGC:               GC -> unit ;
  val XSetArcMode:           GC -> GCArcMode -> unit ;
  val XSetBackground:        GC -> int -> unit ;
  val XSetClipMask:          GC -> Drawable -> unit ;
  val XSetClipOrigin:        GC -> XPoint -> unit ;
  val XSetClipRectangles:    GC -> XPoint -> XRectangle list -> GCOrder -> unit ;
  val XSetDashes:            GC -> int -> int list -> unit ;
  val XSetFillRule:          GC -> GCFillRule -> unit ;
  val XSetFillStyle:         GC -> GCFillStyle -> unit ;
  val XSetFont:              GC -> Font -> unit ;
  val XSetForeground:        GC -> int -> unit ;
  val XSetFunction:          GC -> GCFunction  -> unit ;
  val XSetGraphicsExposures: GC -> bool -> unit ;
  val XSetLineAttributes:    GC -> int -> GCLineStyle -> GCCapStyle -> GCJoinStyle -> unit ;
  val XSetPlaneMask:         GC -> int -> unit ;
  val XSetState:             GC -> int -> int -> GCFunction -> int -> unit ;
  val XSetStipple:           GC -> Drawable -> unit ;
  val XSetSubwindowMode:     GC -> GCSubwindowMode -> unit ;
  val XSetTile:              GC -> Drawable -> unit ;
  val XSetTSOrigin:          GC -> XPoint -> unit ;
  
  (* Images *)
  
  datatype ImageFormat = XYBitmap | XYPixmap | ZPixmap ;
  
  datatype ImageOrder = LSBFirst | MSBFirst ;

  type ImageData ;
  
  val Data: string -> ImageData ;

  datatype XImage = XImage of { data:            ImageData,
                                size:            XRectangle,
                                depth:           int,
                                format:          ImageFormat,
                                xoffset:         int,
                                bitmapPad:       int,
                                byteOrder:       ImageOrder,
                                bitmapUnit:      int,
                                bitsPerPixel:    int,
                                bytesPerLine:    int,
                                visualRedMask:   int,
                                bitmapBitOrder:  ImageOrder,
                                visualBlueMask:  int,
                                visualGreenMask: int } ;

  val XAddPixel:     Display  -> XImage -> int -> unit ;
  val XCreateImage:  Visual   -> int -> ImageFormat -> int -> string -> XRectangle -> int -> int -> XImage ;
  val XGetImage:     Drawable -> XRectangle -> int -> ImageFormat -> XImage ;
  val XGetPixel:     Display  -> XImage -> XPoint -> int ;
  val XGetSubImage:  Drawable -> XRectangle -> int -> ImageFormat -> XImage -> XPoint -> unit ;
  val XPutImage:     Drawable -> GC -> XImage -> XPoint -> XRectangle -> unit ;
  val XPutPixel:     Display  -> XImage -> XPoint -> int -> unit ;
  val XSubImage:     Display  -> XImage -> XRectangle -> XImage ;
      
  val BitmapBitOrder: Display -> ImageOrder ;
  val BitmapPad:      Display -> int ;
  val BitmapUnit:     Display -> int ;
  val ByteOrder:      Display -> ImageOrder ;
  
  val ImageByteOrder: XImage -> ImageOrder ;
  val ImageSize:      XImage -> XRectangle ;
  val ImageDepth:     XImage -> int ;

  (* Keyboard *)
  
  (* keysym values are integers, and are defined in other signatures *)

  val XLookupString: Drawable -> int -> Modifier list -> (string * int) ;

  val XQueryKeymap: Display -> bool list ; (* 256 bools *)

  val IsCursorKey:       int -> bool ;
  val IsFunctionKey:     int -> bool ;
  val IsKeypadKey:       int -> bool ;
  val IsMiscFunctionKey: int -> bool ;
  val IsModifierKey:     int -> bool ;
  val IsPFKey:           int -> bool ;
  
  (* Output Buffer *)
  
  val XFlush: Display -> unit ;
  val XSync:  Display -> bool -> unit ;
  
  (* Pointers *)
  
  val XQueryPointer: Drawable -> (bool * Drawable * Drawable * XPoint * XPoint * Modifier list) ;

  (* Screen Saver *)
  
  datatype SaveMode = ScreenSaverReset | ScreenSaverActive ;

  datatype Blanking = DontPreferBlanking | PreferBlanking | DefaultBlanking ;

  datatype Exposures = DontAllowExposures | AllowExposures | DefaultExposures ;
  
  val XActivateScreenSaver: Display -> unit ;
  val XForceScreenSaver:    Display -> SaveMode -> unit ;
  val XGetScreenSaver:      Display -> (int * int * Blanking * Exposures) ;
  val XResetScreenSaver:    Display -> unit ;
  val XSetScreenSaver:      Display -> int -> int -> Blanking -> Exposures -> unit ;
  
  (* Standard Geometry *)
  
  val XTranslateCoordinates: Drawable -> Drawable -> XPoint -> XPoint * Drawable ;
  
  (* Text *)
  
  val XTextExtents:   XFontStruct -> string   -> (FontDirection * int * int * XCharStruct) ;
  val XTextExtents16: XFontStruct -> int list -> (FontDirection * int * int * XCharStruct) ;
  val XTextWidth:     XFontStruct -> string   -> int ;
  val XTextWidth16:   XFontStruct -> int list -> int ;
  
  (* Tiles, Pixmaps, Stipples and Bitmaps *)
  
  datatype BitmapStatus = BitmapOpenFailed
                        | BitmapFileInvalid
                        | BitmapNoMemory
                        | BitmapSuccess of Drawable * XRectangle * XPoint ;
  
  val XCreateBitmapFromData:       Drawable -> string -> XRectangle -> Drawable ;
  val XCreatePixmap:               Drawable -> XRectangle -> int -> Drawable ;
  val XCreatePixmapFromBitmapData: Drawable -> string -> XRectangle -> int -> int -> int -> Drawable ;
  val XFreePixmap:                 Drawable -> unit ;
  val XQueryBestStipple:           Drawable -> XRectangle -> XRectangle ;
  val XQueryBestTile:              Drawable -> XRectangle -> XRectangle ;
  val XReadBitmapFile:             Drawable -> string -> BitmapStatus ;
  val XWriteBitmapFile:            string -> Drawable -> XRectangle -> XPoint -> BitmapStatus ;
  
  (* User Preferences *)
  
  val XAutoRepeatOff: Display -> unit ;
  val XAutoRepeatOn:  Display -> unit ;
  val XBell:          Display -> int -> unit ;
  val XGetDefault:    Display -> string -> string -> string ;
  
  (* Window Attributes *)
  
  datatype Gravity = ForgetGravity | NorthWestGravity | NorthGravity | NorthEastGravity | WestGravity | CenterGravity
                   | EastGravity   | SouthWestGravity | SouthGravity | SouthEastGravity | StaticGravity ;
  
  val UnmapGravity: Gravity ;    (* same as ForgetGravity *)
  
  datatype XSetWindowAttributes = CWBackPixmap       of Drawable
                                | CWBackPixel        of int
                                | CWBorderPixmap     of Drawable
                                | CWBorderPixel      of int
                                | CWBitGravity       of Gravity
                                | CWWinGravity       of Gravity
                                | CWBackingStore     of BackingStore
                                | CWBackingPlanes    of int
                                | CWBackingPixel     of int
                                | CWOverrideRedirect of bool
                                | CWSaveUnder        of bool
                                | CWEventMask        of EventMask list
                                | CWDontPropagate    of EventMask list
                                | CWColormap         of Colormap
                                | CWCursor           of Cursor ;
  
  datatype WindowClass = CopyFromParentClass | InputOutputClass | InputOnlyClass ;
  
  datatype MapState = IsUnmapped | IsUnviewable | IsViewable ;
  
  datatype XWindowAttributes = XWindowAttributes of
                               {
                                 position:           XPoint,
                                 size:               XRectangle,
                                 borderWidth:        int,
                                 depth:              int,
                                 visual:             Visual,
                                 root:               Drawable,
                                 class:              WindowClass,
                                 bitGravity:         Gravity,
                                 winGravity:         Gravity,
                                 backingStore:       BackingStore,
                                 backingPlanes:      int,
                                 backingPixel:       int,
                                 saveUnder:          bool,
                                 colormap:           Colormap,
                                 mapInstalled:       bool,
                                 mapState:           MapState,
                                 allEventMasks:      EventMask list,
                                 yourEventMask:      EventMask list,
                                 doNotPropagateMask: EventMask list,
                                 overrideRedirect:   bool
                               } ;
  
  val XChangeWindowAttributes:    Drawable -> XSetWindowAttributes list -> unit ;
  val XGetGeometry:               Drawable -> (Drawable * XPoint * XRectangle * int * int) ;
  val XGetWindowAttributes:       Drawable -> XWindowAttributes ;
  val XSetWindowBackground:       Drawable -> int      -> unit ;
  val XSetWindowBackgroundPixmap: Drawable -> Drawable -> unit ;
  val XSetWindowBorder:           Drawable -> int      -> unit ;
  val XSetWindowBorderPixmap:     Drawable -> Drawable -> unit ;
  val XSetWindowBorderWidth:      Drawable -> int      -> unit ;
  
  (* Window Configuration *)
  
  datatype CirculateDirection = RaiseLowest | LowerHighest ;
  
  datatype XWindowChanges = CWPosition    of XPoint
                          | CWSize        of XRectangle
                          | CWBorderWidth of int
                          | CWStackMode   of StackMode
                          | CWSibling     of Drawable ;
  
  val XCirculateSubwindows:     Drawable -> CirculateDirection -> unit ;
  val XCirculateSubwindowsDown: Drawable -> unit ;
  val XCirculateSubwindowsUp:   Drawable -> unit ;
  val XConfigureWindow:         Drawable -> XWindowChanges list -> unit ;
  val XLowerWindow:             Drawable -> unit ;
  val XMapRaised:               Drawable -> unit ;
  val XMapSubwindows:           Drawable -> unit ;
  val XMapWindow:               Drawable -> unit ;
  val XMoveResizeWindow:        Drawable -> XPoint -> XRectangle -> unit ;
  val XMoveWindow:              Drawable -> XPoint -> unit ;
  val XQueryTree:               Drawable -> (Drawable * Drawable * Drawable list) ;
  val XRaiseWindow:             Drawable -> unit ;
  val XReparentWindow:          Drawable -> Drawable -> XPoint -> unit ;
  val XResizeWindow:            Drawable -> XRectangle -> unit ;
  val XRestackWindows:          Drawable list -> unit ;
  val XUnmapSubwindows:         Drawable -> unit ;
  val XUnmapWindow:             Drawable -> unit ;
  
  (* Window Existence *)
  
  val NoDrawable:             Drawable ;
  val NoCursor:               Cursor ;
  val NoFont:                 Font ;
  val NoColormap:             Colormap ;
  val NoVisual:               Visual ;
  val ParentRelative:         Drawable ;
  val CopyFromParentDrawable: Drawable ;
  val CopyFromParentVisual:   Visual ;
  val PointerWindow:          Drawable ;
  val InputFocus:             Drawable ;
  val PointerRoot:            Drawable ;
  val NoSymbol:               int ;
  val AnyKey:                 int ;
  
  val RootWindow:         Display  -> Drawable ;
  val XDestroyWindow:     Drawable -> unit ;
  val XDestroySubwindows: Drawable -> unit ;
  
  val XCreateSimpleWindow: Drawable -> XPoint -> XRectangle -> int -> int -> int -> Drawable ;
  
  val XCreateWindow: Drawable -> XPoint -> XRectangle -> int -> int -> WindowClass -> Visual -> XSetWindowAttributes list -> Drawable ;
  
  (* Window Manager *)
  
  datatype XWMStateHint = DontCareState | NormalState | ZoomState | IconicState | InactiveState ;
  
  datatype XWMHint = InputHint        of bool
                   | StateHint        of XWMStateHint
                   | IconPixmapHint   of Drawable
                   | IconWindowHint   of Drawable
                   | IconPositionHint of XPoint
                   | IconMaskHint     of Drawable ;

  datatype XWMSizeHint = PPosition   of XPoint
                       | PSize       of XRectangle
                       | PMinSize    of XRectangle
                       | PMaxSize    of XRectangle
                       | PResizeInc  of XRectangle
                       | PAspect     of XPoint * XPoint
                       | PBaseSize   of XRectangle
                       | PWinGravity of Gravity ;
  
  datatype XStandardColormap = XStandardColormap of { colormap:  Colormap,
                                                      redMax:    int,
                                                      redMult:   int,
                                                      greenMax:  int,
                                                      greenMult: int,
                                                      blueMax:   int,
                                                      blueMult:  int,
                                                      basePixel: int,
                                                      visual:    Visual } ;

  datatype PropertyValue = PropertyArc         of XArc list
                         | PropertyAtom        of int list
                         | PropertyBitmap      of Drawable list
                         | PropertyColormap    of Colormap list
                         | PropertyCursor      of Cursor list
                         | PropertyDrawable    of Drawable list
                         | PropertyFont        of Font list
                         | PropertyInteger     of int list
                         | PropertyPixmap      of Drawable list
                         | PropertyPoint       of XPoint list
                         | PropertyRectangle   of XRectangle list
                         | PropertyRGBColormap of XStandardColormap list
                         | PropertyString      of string
                         | PropertyVisual      of Visual list
                         | PropertyWindow      of Drawable list
                         | PropertyWMHints     of XWMHint list
                         | PropertyWMSizeHints of XWMSizeHint list 
                         | PropertyWMIconSizes of (XRectangle * XRectangle * XRectangle) list ;
  
  val XSetProperty: Drawable -> int -> PropertyValue -> int -> unit ;
  
  val XGetTextProperty: Drawable -> int -> (string * int * int * int) ;
  
  val XGetWMName:          Drawable -> string ;
  val XGetWMIconName:      Drawable -> string ;
  val XGetWMClientMachine: Drawable -> string ;
  val XSetWMName:          Drawable -> string -> unit ;
  val XSetWMIconName:      Drawable -> string -> unit ;
  val XSetWMClientMachine: Drawable -> string -> unit ;

  val XGetWMCommand: Drawable -> string list ;
  val XGetWMClass:   Drawable -> string list ;
  val XSetWMCommand: Drawable -> string list -> unit ;
  val XSetWMClass:   Drawable -> string list -> unit ;

  val XSetWMHints: Drawable -> XWMHint list -> unit ;
  val XGetWMHints: Drawable -> XWMHint list ;
  
  val XSetWMSizeHints: Drawable -> int -> XWMSizeHint list -> unit ;
  val XGetWMSizeHints: Drawable -> int -> XWMSizeHint list ;

  val XSetWMNormalHints: Drawable -> XWMSizeHint list -> unit ;
  val XGetWMNormalHints: Drawable -> XWMSizeHint list ;
  
  val XWMGeometry: Display -> string -> string -> int -> XWMSizeHint list -> XPoint * XRectangle * Gravity ;

  val XSetWMProperties: Drawable -> string -> string -> string list -> XWMSizeHint list -> XWMHint list -> string list -> unit ;
  
  val XSetIconSizes: Drawable -> (XRectangle * XRectangle * XRectangle) list -> unit ;
  val XGetIconSizes: Drawable -> (XRectangle * XRectangle * XRectangle) list ;
  
  val XSetTransientForHint: Drawable -> Drawable -> unit ;
  val XGetTransientForHint: Drawable -> Drawable ;
  
  val XSetWMColormapWindows: Drawable -> Drawable list -> unit ;
  val XGetWMColormapWindows: Drawable -> Drawable list ;

  val XSetRGBColormaps: Drawable -> int -> XStandardColormap list -> unit ;
  val XGetRGBColormaps: Drawable -> int -> XStandardColormap list ;

  (* Miscellaneous and Convenience functions *)
  
  val ColormapID: Colormap -> int ;                      (* id numbers for X resources *)
  val CursorID:   Cursor   -> int ;                      (* two X resources with the same id are the same resource *)
  val DrawableID: Drawable -> int ;
  val FontID:     Font     -> int ;
  val GCID:       GC       -> int ;
  val VisualID:   Visual   -> int ;
  
  val SameDrawable: Drawable -> Drawable -> bool ;
  
  val ColormapDisplay: Colormap -> Display ;             (* Display associated with X resource *)
  val CursorDisplay:   Cursor   -> Display ;
  val DrawableDisplay: Drawable -> Display ;
  val FontDisplay:     Font     -> Display ;
  val GCDisplay:       GC       -> Display ;
  val VisualDisplay:   Visual   -> Display ;
  
  val ColormapExists: Colormap -> bool ;                 (* value brought across from previous session *)
  val CursorExists:   Cursor   -> bool ;
  val DrawableExists: Drawable -> bool ;
  val FontExists:     Font     -> bool ;
  val GCExists:       GC       -> bool ;
  val VisualExists:   Visual   -> bool ;
  val DisplayExists:  Display  -> bool ;
  
  val XSetColors: GC -> int -> int -> unit ;             (* set foreground and background *)

  val XGetWindowRoot:        Drawable -> Drawable ;      (* root         from XGetGeometry *)
  val XGetWindowPosition:    Drawable -> XPoint ;        (* position     from XGetGeometry *)
  val XGetWindowSize:        Drawable -> XRectangle ;    (* size         from XGetGeometry *)
  val XGetWindowBorderWidth: Drawable -> int ;           (* border width from XGetGeometry *)
  val XGetWindowDepth:       Drawable -> int ;           (* depth        from XGetGeometry *)
  val XGetWindowParent:      Drawable -> Drawable ;      (* parent       from XQueryTree *)
  val XGetWindowChildren:    Drawable -> Drawable list ; (* children     from XQueryTree *)
  
  val GetTimeOfDay: unit -> int * int ;

  val FSFont:          XFontStruct -> Font ;
  val FSDirection:     XFontStruct -> FontDirection ;
  val FSMinChar:       XFontStruct -> int ;
  val FSMaxChar:       XFontStruct -> int ;
  val FSMinByte1:      XFontStruct -> int ;
  val FSMaxByte1:      XFontStruct -> int ;
  val FSAllCharsExist: XFontStruct -> bool ;
  val FSDefaultChar:   XFontStruct -> int ;
  val FSMinBounds:     XFontStruct -> XCharStruct ;
  val FSMaxBounds:     XFontStruct -> XCharStruct ;
  val FSPerChar:       XFontStruct -> XCharStruct list ;
  val FSAscent:        XFontStruct -> int ;
  val FSDescent:       XFontStruct -> int ;
  
  val CharLBearing:   XCharStruct -> int ;
  val CharRBearing:   XCharStruct -> int ;
  val CharWidth:      XCharStruct -> int ;
  val CharAscent:     XCharStruct -> int ;
  val CharDescent:    XCharStruct -> int ;
  val CharAttributes: XCharStruct -> int ;
  
  val FSMinWidth:  XFontStruct -> int ;
  val FSMaxWidth:  XFontStruct -> int ;
  val FSMinHeight: XFontStruct -> int ;
  val FSMaxHeight: XFontStruct -> int ;
  
  val ShiftDown:   Modifier list -> bool ;
  val ControlDown: Modifier list -> bool ;
  
end (* XWINDOWS_SIG *) ;
