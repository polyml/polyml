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


signature importConstTypeSig = 
sig
    type 'a Conversion

    type ArcDirection
    type TextAlign
    type EnumObject
    type DeviceItem
    type DeviceContextFlag
    type DrawTextMode
    type SetWindowPositionStyle
    type StockObjectType
    type RegionOperation
    type SystemPaletteUse
    type MapMode
    type PolyFillMode
    type DialogBoxFlag
    type VirtualKey
    type MenuFlag
    type TrackPopMenu
    type DCBound
    type EnableScrollBarFlag
    type ScrollBarType
    type ScrollWindowFlag
    type GetWindowFlag
    type ShowWindowMode
    type WindowPlacementFlag
    type StretchMode
    type ResultRegion
    type GraphicsMode
    type RasterOperation
    type BitmapOption
    type RasterOpCode
    type XFormType
    type IconIdentifier
    type RedrawWindowFlag
    type BkMode

  structure UpperCaseConversions :
    sig
    val TEXTALIGN : TextAlign Conversion
    val ENUMOBJECT : EnumObject Conversion
    val DEVICEITEM : DeviceItem Conversion
    val DEVICECONTEXTFLAG : DeviceContextFlag Conversion
    val DRAWTEXTMODE : DrawTextMode Conversion
    val SETWINDOWPOSITIONSTYLE : SetWindowPositionStyle Conversion
    val STOCKOBJECTTYPE : StockObjectType Conversion
    val REGIONOPERATION : RegionOperation Conversion
    val SYSTEMPALETTEUSE : SystemPaletteUse Conversion
    val MAPMODE : MapMode Conversion
    val POLYFILLMODE : PolyFillMode Conversion
    val DIALOGBOXFLAG : DialogBoxFlag Conversion
    val VIRTUALKEY : VirtualKey Conversion
    val MENUFLAG : MenuFlag Conversion
    val TRACKPOPMENU : TrackPopMenu Conversion
    val DCBOUND : DCBound Conversion
    val ENABLESCROLLBARFLAG : EnableScrollBarFlag Conversion
    val SCROLLBARTYPE : ScrollBarType Conversion
    val SCROLLWINDOWFLAG : ScrollWindowFlag Conversion
    val GETWINDOWFLAG : GetWindowFlag Conversion
    val SHOWWINDOWMODE : ShowWindowMode Conversion
    val WINDOWPLACEMENTFLAG : WindowPlacementFlag Conversion
    val STRETCHMODE : StretchMode Conversion
    val RESULTREGION : ResultRegion Conversion
    val GRAPHICSMODE : GraphicsMode Conversion
    val RASTEROPERATION : RasterOperation Conversion
    val BITMAPOPTION : BitmapOption Conversion
    val RASTEROPCODE : RasterOpCode Conversion
    val ICONIDENTIFIER : IconIdentifier Conversion
    val REDRAWWINDOWFLAG : RedrawWindowFlag Conversion
    val BKMODE : BkMode Conversion
    val XFORMTYPE : XFormType Conversion
    val ARCDIRECTION : ArcDirection Conversion
  end

end;


signature importCInterfaceSig =
sig
    type vol
    type Ctype
    type 'a Conversion
    type sym
    type dylib

    val alloc       : int -> Ctype -> vol
    val address     : vol -> vol
    val deref       : vol -> vol
    val offset      : int -> Ctype -> vol -> vol
    val assign      : Ctype -> vol -> vol -> unit

    val Cpointer    : Ctype -> Ctype
    val Cvoid       : Ctype

    val breakConversion : 'a Conversion -> (vol -> 'a) * ('a -> vol) * Ctype
    val call_sym     : sym -> (Ctype * vol) list -> Ctype -> vol
    val load_lib     : string -> dylib
    val load_sym     : dylib -> string -> sym

    val CHAR    : string Conversion
    val DOUBLE  : real Conversion
    val FLOAT   : real Conversion
    val INT     : int Conversion
    val LONG    : int Conversion
    val SHORT   : int Conversion
    val STRING  : string Conversion
    val VOID    : unit Conversion
    val BOOL    : bool Conversion
    val POINTER : vol Conversion
end;


signature importTypeSig =
sig 
    type 'a Conversion
    type ForceBackground
    type ClassName
    type WindowName
    type Rectangle
    type Xcoord
    type Ycoord
    type Xoffset
    type Yoffset
    type Width
    type Height
    type Index
    type SuccessState
    type RWpermission
    type MLHWND
    type DCinstance
    type MLHDC
    type MLHGDIOBJ
    type MLHBITMAP
    type MLHRGN
    type MLHBRUSH
    type MLHPALETTE
    type MLHICON
    type MLHMENU
    type MLHDWP
    type Fmf
    type ListBox
    type StaticControl
    type MenuItem

    val HWND : MLHWND Conversion
    val HDC : MLHDC Conversion
    val HGDIOBJ : MLHGDIOBJ Conversion
    val HBITMAP : MLHBITMAP Conversion
    val HRGN : MLHRGN Conversion
    val HBRUSH : MLHBRUSH Conversion
    val HPALETTE : MLHPALETTE Conversion
    val HICON : MLHICON Conversion
    val HMENU : MLHMENU Conversion
    val HDWP : MLHDWP Conversion

    type Point
    type Size
    type XForm

    val POINT : Point Conversion
    val SIZE : Size Conversion
    val XFORM : XForm Conversion
    val RECTANGLE : Rectangle Conversion
    val XCOORD : Xcoord Conversion
    val YCOORD : Ycoord Conversion
    val XOFFSET : Xoffset Conversion
    val YOFFSET : Yoffset Conversion
    val WIDTH : Width Conversion
    val HEIGHT : Height Conversion
    val INDEX : Index Conversion

    val RWPERMISSION : RWpermission Conversion
    val FONTMAPPERFLAG : Fmf Conversion
    val SUCCESSSTATE : SuccessState Conversion
    val DCINSTANCE : DCinstance Conversion
    val FORCEBACKGROUND : ForceBackground Conversion
    val LISTBOX : ListBox Conversion
    val STATICCONTROL : StaticControl Conversion
    val MENUITEM : int Conversion

end;

signature importDirSig =
sig
   val wpolyname   : string 
   val gdilibname  : string
   val userlibname : string
end;

functor GdiFnFct  ( structure Dir : importDirSig
                    structure CInterface : importCInterfaceSig 
                    structure ConstType : importConstTypeSig 
                    structure Type : importTypeSig 
                    
                    sharing type CInterface.Conversion =
                                 ConstType.Conversion =
                                 Type.Conversion  
                  ) = 
struct
    local
        open CInterface
        open Dir

        fun call name = call_sym (load_sym (load_lib gdilibname) name)
        fun rcall_IIIIIIIII name CR (C1,C2,C3,C4,C5,C6,C7,C8,C9) (a1,a2,a3,a4,a5,a6,a7,a8,a9) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (from5,to5,ctype5) = breakConversion C5
                val (from6,to6,ctype6) = breakConversion C6
                val (from7,to7,ctype7) = breakConversion C7
                val (from8,to8,ctype8) = breakConversion C8
                val (from9,to9,ctype9) = breakConversion C9
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = to4 a4
                val va5 = to5 a5
                val va6 = to6 a6
                val va7 = to7 a7
                val va8 = to8 a8
                val va9 = to9 a9
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(ctype4,va4),(ctype5,va5),(ctype6,va6),(ctype7,va7),
                                     (ctype8,va8),(ctype9,va9)] ctypeR
            in  (fromR res)
            end
        fun rcall_IIIIIIIIIIII name CR (C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12) (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (from5,to5,ctype5) = breakConversion C5
                val (from6,to6,ctype6) = breakConversion C6
                val (from7,to7,ctype7) = breakConversion C7
                val (from8,to8,ctype8) = breakConversion C8
                val (from9,to9,ctype9) = breakConversion C9
                val (from10,to10,ctype10) = breakConversion C10
                val (from11,to11,ctype11) = breakConversion C11
                val (from12,to12,ctype12) = breakConversion C12
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = to4 a4
                val va5 = to5 a5
                val va6 = to6 a6
                val va7 = to7 a7
                val va8 = to8 a8
                val va9 = to9 a9
                val va10 = to10 a10
                val va11 = to11 a11
                val va12 = to12 a12
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(ctype4,va4),(ctype5,va5),(ctype6,va6),(ctype7,va7),
                                     (ctype8,va8),(ctype9,va9),(ctype10,va10),(ctype11,va11),(ctype12,va12)] ctypeR
            in  (fromR res)
            end
        fun rcall_IIIIIIIIIII name CR (C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11) (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (from5,to5,ctype5) = breakConversion C5
                val (from6,to6,ctype6) = breakConversion C6
                val (from7,to7,ctype7) = breakConversion C7
                val (from8,to8,ctype8) = breakConversion C8
                val (from9,to9,ctype9) = breakConversion C9
                val (from10,to10,ctype10) = breakConversion C10
                val (from11,to11,ctype11) = breakConversion C11
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = to4 a4
                val va5 = to5 a5
                val va6 = to6 a6
                val va7 = to7 a7
                val va8 = to8 a8
                val va9 = to9 a9
                val va10 = to10 a10
                val va11 = to11 a11
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(ctype4,va4),(ctype5,va5),(ctype6,va6),(ctype7,va7),
                                     (ctype8,va8),(ctype9,va9),(ctype10,va10),(ctype11,va11)] ctypeR
            in  (fromR res)
            end
        fun rcall_IIIIIII name CR (C1,C2,C3,C4,C5,C6,C7) (a1,a2,a3,a4,a5,a6,a7) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (from5,to5,ctype5) = breakConversion C5
                val (from6,to6,ctype6) = breakConversion C6
                val (from7,to7,ctype7) = breakConversion C7
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = to4 a4
                val va5 = to5 a5
                val va6 = to6 a6
                val va7 = to7 a7
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(ctype4,va4),(ctype5,va5),(ctype6,va6),(ctype7,va7)] ctypeR
            in  (fromR res)
            end
        fun rcall_IIIM name CR (C1,C2,C3,C4) (a1,a2,a3,a4) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = address (to4 a4)
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(Cpointer ctype4,va4)] ctypeR
            in  (fromR res,from4 (deref va4))
            end
        fun rcall_IM name CR (C1,C2) (a1,a2) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = address (to2 a2)
                val res = call name [(ctype1,va1),(Cpointer ctype2,va2)] ctypeR
            in  (fromR res,from2 (deref va2))
            end
        fun rcall_WRR name CR (C1,C2,C3) (a2,a3) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = address (alloc 1 ctype1)
                val va2 = address (to2 a2)
                val va3 = address (to3 a3)
                val res = call name [(Cpointer ctype1,va1),(Cpointer ctype2,va2),(Cpointer ctype3,va3)] ctypeR
            in  (fromR res,from1 (deref va1))
            end
        fun rcall_IRI name CR (C1,C2,C3) (a1,a2,a3) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = address (to2 a2)
                val va3 = to3 a3
                val res = call name [(ctype1,va1),(Cpointer ctype2,va2),(ctype3,va3)] ctypeR
            in  (fromR res)
            end
        fun rcall_IR name CR (C1,C2) (a1,a2) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = address (to2 a2)
                val res = call name [(ctype1,va1),(Cpointer ctype2,va2)] ctypeR
            in  (fromR res)
            end
        fun rcall_IIRI name CR (C1,C2,C3,C4) (a1,a2,a3,a4) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = address (to3 a3)
                val va4 = to4 a4
                val res = call name [(ctype1,va1),(ctype2,va2),(Cpointer ctype3,va3),(ctype4,va4)] ctypeR
            in  (fromR res)
            end
        fun rcall_IIIIII name CR (C1,C2,C3,C4,C5,C6) (a1,a2,a3,a4,a5,a6) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (from5,to5,ctype5) = breakConversion C5
                val (from6,to6,ctype6) = breakConversion C6
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = to4 a4
                val va5 = to5 a5
                val va6 = to6 a6
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(ctype4,va4),(ctype5,va5),(ctype6,va6)] ctypeR
            in  (fromR res)
            end
        fun rcall_IIIW name CR (C1,C2,C3,C4) (a1,a2,a3) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = address (alloc 1 ctype4)
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(Cpointer ctype4,va4)] ctypeR
            in  (fromR res,from4 (deref va4))
            end
        fun rcall_IIW name CR (C1,C2,C3) (a1,a2) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = address (alloc 1 ctype3)
                val res = call name [(ctype1,va1),(ctype2,va2),(Cpointer ctype3,va3)] ctypeR
            in  (fromR res,from3 (deref va3))
            end
        fun rcall_IIII name CR (C1,C2,C3,C4) (a1,a2,a3,a4) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = to4 a4
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(ctype4,va4)] ctypeR
            in  (fromR res)
            end
        fun rcall_IIIII name CR (C1,C2,C3,C4,C5) (a1,a2,a3,a4,a5) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (from5,to5,ctype5) = breakConversion C5
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = to4 a4
                val va5 = to5 a5
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(ctype4,va4),(ctype5,va5)] ctypeR
            in  (fromR res)
            end
        fun rcall_I name CR (C1) (a1) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val res = call name [(ctype1,va1)] ctypeR
            in  (fromR res)
            end
        fun rcall_III name CR (C1,C2,C3) (a1,a2,a3) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3)] ctypeR
            in  (fromR res)
            end
        fun rcall_IW name CR (C1,C2) (a1) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = address (alloc 1 ctype2)
                val res = call name [(ctype1,va1),(Cpointer ctype2,va2)] ctypeR
            in  (fromR res,from2 (deref va2))
            end
        fun rcall_II name CR (C1,C2) (a1,a2) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val res = call name [(ctype1,va1),(ctype2,va2)] ctypeR
            in  (fromR res)
            end
    in
        open ConstType
        open UpperCaseConversions 
        open Type
        val BitBlt                     = rcall_IIIIIIIII "BitBlt" SUCCESSSTATE
                                         (HDC,XCOORD,YCOORD,WIDTH,HEIGHT,HDC,XCOORD,YCOORD,RASTEROPERATION)
        val CreateCompatibleBitmap     = rcall_III "CreateCompatibleBitmap" HBITMAP (HDC,WIDTH,HEIGHT)
        val GetGraphicsMode            = rcall_I "GetGraphicsMode" GRAPHICSMODE (HDC)
        val GetStretchBltMode          = rcall_I "GetStretchBltMode" STRETCHMODE (HDC)
        val MaskBlt                    = rcall_IIIIIIIIIIII "MaskBlt" SUCCESSSTATE
                                         (HDC,XCOORD,YCOORD,WIDTH,HEIGHT,HDC,XCOORD,YCOORD,HBITMAP,XOFFSET,YOFFSET,RASTEROPCODE)
        val PatBlt                     = rcall_IIIIII "PatBlt" SUCCESSSTATE (HDC,XCOORD,YCOORD,WIDTH,HEIGHT,RASTEROPCODE)
        val SetStretchBltMode          = rcall_II "SetStretchBltMode" SUCCESSSTATE (HDC,STRETCHMODE)
        val StretchBlt                 = rcall_IIIIIIIIIII "StretchBlt" SUCCESSSTATE
                                         (HDC,XCOORD,YCOORD,WIDTH,HEIGHT,HDC,XCOORD,YCOORD,WIDTH,HEIGHT,RASTEROPCODE)
        val PlgBlt                     = rcall_IIIIIII "PlgBlt" SUCCESSSTATE (HDC,RECTANGLE,HDC,RECTANGLE,HBITMAP,XCOORD,YCOORD)
        val SetBitmapDimensionEx       = rcall_IIIM "SetBitmapDimensionEx" SUCCESSSTATE (HBITMAP,WIDTH,HEIGHT,SIZE)
        val GetBitmapDimensionEx       = rcall_IW "GetBitmapDimensionEx" SUCCESSSTATE (HBITMAP,SIZE)
        val GetBrushOrgEx              = rcall_IW "GetBrushOrgEx" SUCCESSSTATE (HDC,POINT)
        val SetBrushOrgEx              = rcall_IM "SetBrushOrgEx" SUCCESSSTATE (HDC,POINT)
        val CreatePatternBrush         = rcall_I "CreatePatternBrush" HBRUSH (HBITMAP)
        val GetStockObject             = rcall_I "GetStockObject" HGDIOBJ (STOCKOBJECTTYPE)
        val GetClipBox                 = rcall_IW "GetClipBox" RESULTREGION (HDC,RECTANGLE)
        val RectVisible                = rcall_IR "RectVisible" BOOL (HDC,RECTANGLE)
        val ExtSelectClipRgn           = rcall_III "ExtSelectClipRgn" RESULTREGION (HDC,HRGN,REGIONOPERATION)
        val GetClipRgn                 = rcall_II "GetClipRgn" SUCCESSSTATE (HDC,HRGN)
        val GetMetaRgn                 = rcall_II "GetMetaRgn" SUCCESSSTATE (HDC,HRGN)
        val OffsetClipRgn              = rcall_III "OffsetClipRgn" RESULTREGION (HDC,XOFFSET,YOFFSET)
        val PtVisible                  = rcall_IIII "PtVisible" BOOL (HDC,XCOORD,YCOORD,BOOL)
        val SelectClipPath             = rcall_II "SelectClipPath" SUCCESSSTATE (HDC,REGIONOPERATION)
        val SelectClipRgn              = rcall_II "SelectClipRgn" SUCCESSSTATE (HDC,HRGN)
        val SetMetaRgn                 = rcall_I "SetMetaRgn" SUCCESSSTATE (HDC)
        val GetSystemPaletteUse        = rcall_I "GetSystemPaletteUse" SYSTEMPALETTEUSE (HDC)
        val RealizePalette             = rcall_I "RealizePalette" INT (HDC)
        val ResizePalette              = rcall_II "ResizePalette" SUCCESSSTATE (HPALETTE,INT)
        val SelectPalette              = rcall_III "SelectPalette" HPALETTE (HDC,HPALETTE,FORCEBACKGROUND)
        val SetSystemPaletteUse        = rcall_II "SetSystemPaletteUse" SYSTEMPALETTEUSE (HDC,SYSTEMPALETTEUSE)
        val UpdateColors               = rcall_I "UpdateColors" SUCCESSSTATE (HDC)
        val CreateHalftonePalette      = rcall_I "CreateHalftonePalette" HPALETTE (HDC)
        val GetMapMode                 = rcall_I "GetMapMode" MAPMODE (HDC)
        val SetMapMode                 = rcall_III "SetMapMode" MAPMODE (HDC,MAPMODE,MAPMODE)
        val ClientToScreen             = rcall_IM "ClientToScreen" SUCCESSSTATE (HWND,POINT)
        val CombineTransform           = rcall_WRR "CombineTransform" SUCCESSSTATE (XFORM,XFORM,XFORM)
        val GetCurrentPositionEx       = rcall_IW "GetCurrentPositionEx" SUCCESSSTATE (HDC,POINT)
        val GetViewportExtEx           = rcall_IW "GetViewportExtEx" SUCCESSSTATE (HDC,SIZE)
        val GetViewportOrgEx           = rcall_IW "GetViewportOrgEx" SUCCESSSTATE (HDC,POINT)
        val GetWindowExtEx             = rcall_IW "GetWindowExtEx" SUCCESSSTATE (HDC,SIZE)
        val GetWindowOrgEx             = rcall_IW "GetWindowOrgEx" SUCCESSSTATE (HDC,POINT)
        val GetWorldTransform          = rcall_IW "GetWorldTransform" SUCCESSSTATE (HDC,XFORM)
        val ModifyWorldTransform       = rcall_IRI "ModifyWorldTransform" SUCCESSSTATE (HDC,XFORM,XFORMTYPE)
        val OffsetViewportOrgEx        = rcall_IIIW "OffsetViewportOrgEx" SUCCESSSTATE (HDC,XOFFSET,YOFFSET,POINT)
        val OffsetWindowOrgEx          = rcall_IIIW "OffsetWindowOrgEx" SUCCESSSTATE (HDC,XOFFSET,YOFFSET,POINT)
        val ScreenToClient             = rcall_IR "ScreenToClient" SUCCESSSTATE (HWND,POINT)
        val SetViewportExtEx           = rcall_IIIW "SetViewportExtEx" SUCCESSSTATE (HDC,INT,INT,SIZE)
        val SetViewportOrgEx           = rcall_IIIW "SetViewportOrgEx" SUCCESSSTATE (HDC,INT,INT,POINT)
        val SetWindowExtEx             = rcall_IIIW "SetWindowExtEx" SUCCESSSTATE (HDC,INT,INT,SIZE)
        val SetWindowOrgEx             = rcall_IIIW "SetWindowOrgEx" SUCCESSSTATE (HDC,INT,INT,POINT)
        val SetWorldTransform          = rcall_IR "SetWorldTransform" SUCCESSSTATE (HDC,XFORM)
        val CancelDC                   = rcall_I "CancelDC" SUCCESSSTATE (HDC)
        val CreateCompatibleDC         = rcall_I "CreateCompatibleDC" HDC (HDC)
        val DeleteDC                   = rcall_I "DeleteDC" SUCCESSSTATE (HDC)
        val DeleteObject               = rcall_I "DeleteObject" SUCCESSSTATE (HGDIOBJ)
        val GetCurrentObject           = rcall_II "GetCurrentObject" HGDIOBJ (HDC,ENUMOBJECT)
        val GetDeviceCaps              = rcall_II "GetDeviceCaps" DEVICEITEM (HDC,DEVICEITEM)
        val GetObjectType              = rcall_I "GetObjectType" ENUMOBJECT (HGDIOBJ)
        val RestoreDC                  = rcall_II "RestoreDC" SUCCESSSTATE (HDC,DCINSTANCE)
        val SaveDC                     = rcall_I "SaveDC" INT (HDC)
        val SelectObject               = rcall_II "SelectObject" RESULTREGION (HDC,HGDIOBJ)
        val GetDCEx                    = rcall_III "GetDCEx" HDC (HWND,HRGN,DEVICECONTEXTFLAG)
        val GetDCOrgEx                 = rcall_IW "GetDCOrgEx" SUCCESSSTATE (HDC,POINT)
        val FillRect                   = rcall_IRI "FillRect" SUCCESSSTATE (HDC,RECTANGLE,HBRUSH)
        val FrameRect                  = rcall_IRI "FrameRect" SUCCESSSTATE (HDC,RECTANGLE,HBRUSH)
        val InvertRect                 = rcall_IR "InvertRect" SUCCESSSTATE (HDC,RECTANGLE)
        val AddFontResource            = rcall_I "AddFontResource" INT (STRING)
        val CreateScalableFontResource = rcall_IIII "CreateScalableFontResource" SUCCESSSTATE
                                         (RWPERMISSION,STRING,STRING,STRING)
        val GetTextAlign               = rcall_I "GetTextAlign" TEXTALIGN (HDC)
        val GetTextCharacterExtra      = rcall_I "GetTextCharacterExtra" INT (HDC)
        val RemoveFontResource         = rcall_II "RemoveFontResource" SUCCESSSTATE (STRING,SUCCESSSTATE)
        val SetMapperFlags             = rcall_II "SetMapperFlags" FONTMAPPERFLAG (HDC,FONTMAPPERFLAG)
        val SetTextAlign               = rcall_II "SetTextAlign" TEXTALIGN (HDC,TEXTALIGN)
        val SetTextCharacterExtra      = rcall_II "SetTextCharacterExtra" INT (HDC,INT)
        val SetTextJustification       = rcall_III "SetTextJustification" SUCCESSSTATE (HDC,INT,INT)
        val DrawText                   = rcall_IIRI "DrawText" HEIGHT (HDC,STRING,RECTANGLE,DRAWTEXTMODE)
        val GetAspectRatioFilterEx     = rcall_IW "GetAspectRatioFilterEx" SUCCESSSTATE (HDC,SIZE)
        val AngleArc                   = rcall_IIIIII "AngleArc" SUCCESSSTATE (HDC,XCOORD,YCOORD,INT,FLOAT,FLOAT)
        val Arc                        = rcall_IIIIII "Arc" SUCCESSSTATE (HDC,RECTANGLE,INT,INT,INT,INT)
        val ArcTo                      = rcall_IIIIII "ArcTo" SUCCESSSTATE (HDC,RECTANGLE,INT,INT,INT,INT)
        val GetArcDirection            = rcall_I "GetArcDirection" ARCDIRECTION (HDC)
        val SetArcDirection            = rcall_II "SetArcDirection" ARCDIRECTION (HDC,ARCDIRECTION)
        val MoveToEx                   = rcall_IIIW "MoveToEx" SUCCESSSTATE (HDC,XCOORD,YCOORD,POINT)
        val AbortPath                  = rcall_I "AbortPath" SUCCESSSTATE (HDC)
        val BeginPath                  = rcall_I "BeginPath" SUCCESSSTATE (HDC)
        val CloseFigure                = rcall_I "CloseFigure" SUCCESSSTATE (HDC)
        val EndPath                    = rcall_I "EndPath" SUCCESSSTATE (HDC)
        val FillPath                   = rcall_I "FillPath" SUCCESSSTATE (HDC)
        val FlattenPath                = rcall_I "FlattenPath" SUCCESSSTATE (HDC)
        val GetMiterLimit              = rcall_IW "GetMiterLimit" SUCCESSSTATE (HDC,FLOAT)
        val PathToRegion               = rcall_I "PathToRegion" HRGN (HDC)
        val SetMiterLimit              = rcall_IIW "SetMiterLimit" SUCCESSSTATE (HDC,FLOAT,FLOAT)
        val StrokeAndFillPath          = rcall_I "StrokeAndFillPath" SUCCESSSTATE (HDC)
        val StrokePath                 = rcall_I "StrokePath" SUCCESSSTATE (HDC)
        val WidenPath                  = rcall_I "WidenPath" SUCCESSSTATE (HDC)
        val CombineRgn                 = rcall_IIII "CombineRgn" RESULTREGION (HRGN,HRGN,HRGN,REGIONOPERATION)
        val EqualRgn                   = rcall_II "EqualRgn" BOOL (HRGN,HRGN)
        val FillRgn                    = rcall_III "FillRgn" SUCCESSSTATE (HDC,HRGN,HBRUSH)
        val FrameRgn                   = rcall_IIIII "FrameRgn" SUCCESSSTATE (HDC,HRGN,HBRUSH,WIDTH,HEIGHT)
        val GetPolyFillMode            = rcall_I "GetPolyFillMode" POLYFILLMODE (HDC)
        val InvertRgn                  = rcall_II "InvertRgn" SUCCESSSTATE (HDC,HRGN)
        val OffsetRgn                  = rcall_III "OffsetRgn" RESULTREGION (HRGN,XOFFSET,YOFFSET)
        val PaintRgn                   = rcall_II "PaintRgn" SUCCESSSTATE (HDC,HRGN)
        val PtInRegion                 = rcall_III "PtInRegion" BOOL (HRGN,XCOORD,YCOORD)
        val SetPolyFillMode            = rcall_II "SetPolyFillMode" POLYFILLMODE (HDC,POLYFILLMODE)
        val GetRgnBox                  = rcall_IW "GetRgnBox" SUCCESSSTATE (HRGN,RECTANGLE)
        val RectInRegion               = rcall_II "RectInRegion" BOOL (HRGN,RECTANGLE)
    end
end
