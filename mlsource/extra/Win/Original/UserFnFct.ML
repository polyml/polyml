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
    type MessageBoxValue
    type MessageBoxIdentifier

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
    val MESSAGEBOXVALUE : MessageBoxValue Conversion
    val MESSAGEBOXIDENTIFIER : MessageBoxIdentifier Conversion 
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

functor UserFnFct ( structure Dir : importDirSig
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

        fun call name = call_sym (load_sym (load_lib userlibname) name)
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
        fun rcall_IIIIIR name CR (C1,C2,C3,C4,C5,C6) (a1,a2,a3,a4,a5,a6) =
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
                val va6 = address (to6 a6)
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(ctype4,va4),(ctype5,va5),(Cpointer ctype6,va6)] ctypeR
            in  (fromR res)
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
        fun rcall_IRII name CR (C1,C2,C3,C4) (a1,a2,a3,a4) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = address (to2 a2)
                val va3 = to3 a3
                val va4 = to4 a4
                val res = call name [(ctype1,va1),(Cpointer ctype2,va2),(ctype3,va3),(ctype4,va4)] ctypeR
            in  (fromR res)
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
        fun vcall_III name (C1,C2,C3) (a1,a2,a3) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3)] Cvoid
            in  ()
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
        fun rcall_IIIRRIW name CR (C1,C2,C3,C4,C5,C6,C7) (a1,a2,a3,a4,a5,a6) =
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
                val va4 = address (to4 a4)
                val va5 = address (to5 a5)
                val va6 = to6 a6
                val va7 = address (alloc 1 ctype7)
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(Cpointer ctype4,va4),(Cpointer ctype5,va5),
                                     (ctype6,va6),(Cpointer ctype7,va7)] ctypeR
            in  (fromR res,from7 (deref va7))
            end
        fun rcall_IIIRR name CR (C1,C2,C3,C4,C5) (a1,a2,a3,a4,a5) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (from5,to5,ctype5) = breakConversion C5
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = address (to4 a4)
                val va5 = address (to5 a5)
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(Cpointer ctype4,va4),(Cpointer ctype5,va5)] ctypeR
            in  (fromR res)
            end
        fun rcall_IIIRRIIW name CR (C1,C2,C3,C4,C5,C6,C7,C8) (a1,a2,a3,a4,a5,a6,a7) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (from5,to5,ctype5) = breakConversion C5
                val (from6,to6,ctype6) = breakConversion C6
                val (from7,to7,ctype7) = breakConversion C7
                val (from8,to8,ctype8) = breakConversion C8
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = address (to4 a4)
                val va5 = address (to5 a5)
                val va6 = to6 a6
                val va7 = to7 a7
                val va8 = address (alloc 1 ctype8)
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(Cpointer ctype4,va4),(Cpointer ctype5,va5),
                                     (ctype6,va6),(ctype7,va7),(Cpointer ctype8,va8)] ctypeR
            in  (fromR res,from8 (deref va8))
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
        fun rcall_RR name CR (C1,C2) (a1,a2) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = address (to1 a1)
                val va2 = address (to2 a2)
                val res = call name [(Cpointer ctype1,va1),(Cpointer ctype2,va2)] ctypeR
            in  (fromR res)
            end
        fun rcall_WRRI name CR (C1,C2,C3,C4) (a2,a3,a4) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = address (alloc 1 ctype1)
                val va2 = address (to2 a2)
                val va3 = address (to3 a3)
                val va4 = to4 a4
                val res = call name [(Cpointer ctype1,va1),(Cpointer ctype2,va2),(Cpointer ctype3,va3),(ctype4,va4)] ctypeR
            in  (fromR res,from1 (deref va1))
            end
        fun rcall_R name CR (C1) (a1) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = address (to1 a1)
                val res = call name [(Cpointer ctype1,va1)] ctypeR
            in  (fromR res)
            end
        fun rcall_MII name CR (C1,C2,C3) (a1,a2,a3) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = address (to1 a1)
                val va2 = to2 a2
                val va3 = to3 a3
                val res = call name [(Cpointer ctype1,va1),(ctype2,va2),(ctype3,va3)] ctypeR
            in  (fromR res,from1 (deref va1))
            end
        fun rcall_RI name CR (C1,C2) (a1,a2) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = address (to1 a1)
                val va2 = to2 a2
                val res = call name [(Cpointer ctype1,va1),(ctype2,va2)] ctypeR
            in  (fromR res)
            end
        fun rcall_IIIIW name CR (C1,C2,C3,C4,C5) (a1,a2,a3,a4) =
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
                val va5 = address (alloc 1 ctype5)
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(ctype4,va4),(Cpointer ctype5,va5)] ctypeR
            in  (fromR res,from5 (deref va5))
            end
        fun rcall_W name CR (C1) () =
            let val (from1,to1,ctype1) = breakConversion C1
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = address (alloc 1 ctype1)
                val res = call name [(Cpointer ctype1,va1)] ctypeR
            in  (fromR res,from1 (deref va1))
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
        fun rcall_IIIIIIII name CR (C1,C2,C3,C4,C5,C6,C7,C8) (a1,a2,a3,a4,a5,a6,a7,a8) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (from5,to5,ctype5) = breakConversion C5
                val (from6,to6,ctype6) = breakConversion C6
                val (from7,to7,ctype7) = breakConversion C7
                val (from8,to8,ctype8) = breakConversion C8
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = to4 a4
                val va5 = to5 a5
                val va6 = to6 a6
                val va7 = to7 a7
                val va8 = to8 a8
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(ctype4,va4),(ctype5,va5),(ctype6,va6),(ctype7,va7),
                                     (ctype8,va8)] ctypeR
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
        fun rcall_O name CR (C0) () =
            let val (fromR,toR,ctypeR) = breakConversion CR
                val res = call name [] ctypeR
            in  (fromR res)
            end
        fun rcall_I name CR (C1) (a1) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val res = call name [(ctype1,va1)] ctypeR
            in  (fromR res)
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
        fun rcall_IIIIIW name CR (C1,C2,C3,C4,C5,C6) (a1,a2,a3,a4,a5) =
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
                val va6 = address (alloc 1 ctype6)
                val res = call name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(ctype4,va4),(ctype5,va5),(Cpointer ctype6,va6)] ctypeR
            in  (fromR res,from6 (deref va6))
            end
    in
        open ConstType
        open UpperCaseConversions 
        open Type

        val LoadBitmap             = rcall_II "LoadBitmapA" HBITMAP (BITMAPOPTION,STRING)
        val CheckDlgButton         = rcall_III "CheckDlgButton" SUCCESSSTATE (HWND,INT,INT)
        val CheckRadioButton       = rcall_IIII "CheckRadioButton" SUCCESSSTATE (HWND,INT,INT,INT)
        val IsDlgButtonChecked     = rcall_II "IsDlgButtonChecked" INT (HWND,INT)
        val DlgDirListComboBox     = rcall_IIIII "DlgDirListComboBox" SUCCESSSTATE (HWND,STRING,INT,INT,DIALOGBOXFLAG)
        val DlgDirSelectComboBoxEx = rcall_IIII "DlgDirSelectComboBoxEx" SUCCESSSTATE (HWND,STRING,INT,INT)
        val DlgDirSelectEx         = rcall_IIII "DlgDirSelectEx" SUCCESSSTATE (HWND,STRING,INT,INT)
        val ArrangeIconicWindows   = rcall_I "ArrangeIconicWindows" INT (HWND)
        val DestroyIcon            = rcall_I "DestroyIcon" SUCCESSSTATE (HICON)
        val DrawIcon               = rcall_IIII "DrawIcon" SUCCESSSTATE (HDC,XCOORD,YCOORD,HICON)
        val IsIconic               = rcall_I "IsIconic" BOOL (HWND)
        val OpenIcon               = rcall_I "OpenIcon" SUCCESSSTATE (HWND)
        val EnableWindow           = rcall_II "EnableWindow" SUCCESSSTATE (HWND,BOOL)
        val GetDC                  = rcall_I "GetDC" HDC (HWND)
        val ReleaseDC              = rcall_II "ReleaseDC" BOOL (HWND,HDC)
        val GetActiveWindow        = rcall_O "GetActiveWindow" HWND (VOID)
        val GetFocus               = rcall_O "GetFocus" HWND (VOID)
        val IsWindowEnabled        = rcall_I "IsWindowEnabled" BOOL (HWND)
        val RegisterHotKey         = rcall_IIII "RegisterHotKey" SUCCESSSTATE (HWND,INT,INT,VIRTUALKEY)
        val SetActiveWindow        = rcall_I "SetActiveWindow" HWND (HWND)
        val SetFocus               = rcall_I "SetFocus" HWND (HWND)
        val AppendMenu             = rcall_IIII "AppendMenuA" SUCCESSSTATE (HMENU,MENUFLAG,INT,STRING)
        val AppendSubMenu          = rcall_IIII "AppendMenuA" SUCCESSSTATE (HMENU,MENUFLAG,HMENU,STRING)
        val CheckMenuItem          = rcall_III "CheckMenuItem" MENUFLAG (HMENU,INT,MENUFLAG)
        val CreateMenu             = rcall_O "CreateMenu" HMENU (VOID)
        val CreatePopupMenu        = rcall_O "CreatePopupMenu" HMENU (VOID)
        val DeleteMenu             = rcall_III "DeleteMenu" SUCCESSSTATE (HMENU,INT,MENUFLAG)
        val DestroyMenu            = rcall_I "DestroyMenu" SUCCESSSTATE (HMENU)
        val DrawMenuBar            = rcall_I "DrawMenuBar" SUCCESSSTATE (HWND)
        val EnableMenuItem         = rcall_III "EnableMenuItem" MENUFLAG (HMENU,INT,MENUFLAG)
        val GetMenu                = rcall_I "GetMenu" HMENU (HWND)
        val GetMenuItemCount       = rcall_I "GetMenuItemCount" INT (HMENU)
        val GetMenuItemID          = rcall_II "GetMenuItemID" INT (HMENU,INT)
        val GetMenuState           = rcall_III "GetMenuState" MENUFLAG (HMENU,INT,MENUFLAG)
        val GetSubMenu             = rcall_II "GetSubMenu" HMENU (HMENU,INT)
        val GetSystemMenu          = rcall_II "GetSystemMenu" HMENU (HWND,BOOL)
        val HiliteMenuItem         = rcall_IIII "HiliteMenuItem" BOOL (HWND,HMENU,INT,MENUFLAG)
        val IsMenu                 = rcall_I "IsMenu" BOOL (HMENU)
        val SetMenu                = rcall_II "SetMenu" SUCCESSSTATE (HWND,HMENU)
        val SetMenuItemBitmaps     = rcall_IIIII "SetMenuItemBitmaps" SUCCESSSTATE (HMENU,INT,MENUFLAG,HBITMAP,HBITMAP)
        val InsertMenu             = rcall_IIIII "InsertMenuA" SUCCESSSTATE (HMENU,MENUITEM,MENUFLAG,MENUITEM,STRING)
        val ModifyMenu             = rcall_IIIII "ModifyMenuA" SUCCESSSTATE (HMENU,MENUITEM,MENUFLAG,MENUITEM,STRING)
        val RemoveMenu             = rcall_III "RemoveMenu" SUCCESSSTATE (HMENU,MENUITEM,MENUFLAG)
        val TrackPopupMenu         = rcall_IIIIIR "TrackPopupMenu" SUCCESSSTATE
                                     (HMENU,TRACKPOPMENU,XCOORD,YCOORD,HWND,RECTANGLE)
        val GetCapture             = rcall_O "GetCapture" HWND (VOID)
        val ReleaseCapture         = rcall_O "ReleaseCapture" SUCCESSSTATE (VOID)
        val SetCapture             = rcall_I "SetCapture" HWND (HWND)
        val SwapMouseButton        = rcall_I "SwapMouseButton" SUCCESSSTATE (BOOL)
        val DrawFocusRect          = rcall_IR "DrawFocusRect" SUCCESSSTATE (HDC,RECTANGLE)
        val GetBoundsRect          = rcall_IIW "GetBoundsRect" DCBOUND (HDC,DCBOUND,RECTANGLE)
        val InvalidateRect         = rcall_IRI "InvalidateRect" SUCCESSSTATE (HWND,RECTANGLE,BOOL)
        val RedrawWindow           = rcall_IRII "RedrawWindow" SUCCESSSTATE (HWND,RECTANGLE,HRGN,REDRAWWINDOWFLAG)
        val SetBoundsRect          = rcall_IRI "SetBoundsRect" DCBOUND (HDC,RECTANGLE,DCBOUND)
        val ValidateRect           = rcall_IR "ValidateRect" SUCCESSSTATE (HWND,RECTANGLE)
        val ExcludeUpdateRgn       = rcall_II "ExcludeUpdateRgn" RESULTREGION (HDC,HWND)
        val GdiFlush               = rcall_O "GdiFlush" SUCCESSSTATE (VOID)
        val GdiGetBatchLimit       = rcall_O "GdiGetBatchLimit" INT (VOID)
        val GdiSetBatchLimit       = rcall_I "GdiSetBatchLimit" INT (INT)
        val GetBkMode              = rcall_I "GetBkMode" BKMODE (HDC)
        val GetROP2                = rcall_I "GetROP2" RASTEROPERATION (HDC)
        val GetUpdateRgn           = rcall_III "GetUpdateRgn" RESULTREGION (HWND,HRGN,BOOL)
        val GetWindowDC            = rcall_I "GetWindowDC" HDC (HWND)
        val InvalidateRgn          = vcall_III "InvalidateRgn" (HWND,HRGN,BOOL)
        val LockWindowUpdate       = rcall_I "LockWindowUpdate" SUCCESSSTATE (HWND)
        val SetBkMode              = rcall_II "SetBkMode" BKMODE (HDC,BKMODE)
        val SetROP2                = rcall_II "SetROP2" RASTEROPERATION (HDC,RASTEROPERATION)
        val UpdateWindow           = rcall_I "UpdateWindow" SUCCESSSTATE (HWND)
        val ValidateRgn            = rcall_II "ValidateRgn" SUCCESSSTATE (HWND,HRGN)
        val WindowFromDC           = rcall_I "WindowFromDC" HWND (HDC)
        val EnableScrollBar        = rcall_III "EnableScrollBar" SUCCESSSTATE (HWND,SCROLLBARTYPE,ENABLESCROLLBARFLAG)
        val GetScrollPos           = rcall_II "GetScrollPos" INT (HWND,SCROLLBARTYPE)
        val SetScrollRange         = rcall_IIIII "SetScrollRange" BOOL (HWND,SCROLLBARTYPE,INT,INT,BOOL) 
        val SetScrollPos           = rcall_IIII "SetScrollPos" INT (HWND,SCROLLBARTYPE,INT,BOOL)
        val ShowScrollBar          = rcall_III "ShowScrollBar" SUCCESSSTATE (HWND,SCROLLBARTYPE,BOOL)
        val ScrollDC               = rcall_IIIRRIW "ScrollDC" SUCCESSSTATE (HDC,INT,INT,RECTANGLE,RECTANGLE,HRGN,RECTANGLE)
        val ScrollWindow           = rcall_IIIRR "ScrollWindow" SUCCESSSTATE (HWND,INT,INT,RECTANGLE,RECTANGLE)
        val ScrollWindowEx         = rcall_IIIRRIIW "ScrollWindowEx" SUCCESSSTATE
                                     (HWND,INT,INT,RECTANGLE,RECTANGLE,HRGN,SCROLLWINDOWFLAG,RECTANGLE)
        val DlgDirList             = rcall_IIIIII "DlgDirListA" SUCCESSSTATE
                                     (HWND,STRING,LISTBOX,STATICCONTROL,DIALOGBOXFLAG,STRING)
        val DlgDirSelectEx         = rcall_III "DlgDirSelectExA" SUCCESSSTATE (HWND,LISTBOX,STRING)
        val EqualRect              = rcall_RR "EqualRect" BOOL (RECTANGLE,RECTANGLE)
        val InflateRect            = rcall_MII "InflateRect" SUCCESSSTATE (RECTANGLE,INT,INT)
        val IntersectRect          = rcall_WRRI "IntersectRect" BOOL (RECTANGLE,RECTANGLE,RECTANGLE,BOOL)
        val IsRectEmpty            = rcall_R "IsRectEmpty" BOOL (RECTANGLE)
        val OffsetRect             = rcall_MII "OffsetRect" SUCCESSSTATE (RECTANGLE,XOFFSET,YOFFSET)
        val PtInRect               = rcall_RI "PtInRect" BOOL (RECTANGLE,POINT)
        val SetRect                = rcall_IIIIW "SetRect" SUCCESSSTATE (INT,INT,INT,INT,RECTANGLE)
        val SetRectEmpty           = rcall_W "SetRectEmpty" SUCCESSSTATE (RECTANGLE)
        val SubtractRect           = rcall_WRR "SubtractRect" SUCCESSSTATE (RECTANGLE,RECTANGLE,RECTANGLE)
        val UnionRect              = rcall_WRR "UnionRect" SUCCESSSTATE (RECTANGLE,RECTANGLE,RECTANGLE)
        val AnyPopup               = rcall_O "AnyPopup" BOOL (VOID)
        val ArrangeIconicWindows   = rcall_I "ArrangeIconicWindows" HEIGHT (HWND)
        val BeginDeferWindowPos    = rcall_I "BeginDeferWindowPos" HDWP (INT)
        val BringWindowToTop       = rcall_I "BringWindowToTop" SUCCESSSTATE (HWND)
        val ChildWindowFromPoint   = rcall_II "ChildWindowFromPoint" HWND (HWND,POINT)
        val CloseWindow            = rcall_I "CloseWindow" SUCCESSSTATE (HWND)
        val DeferWindowPos         = rcall_IIIIIIII "DeferWindowPos" HDWP
                                     (HDWP,HWND,HWND,XCOORD,YCOORD,WIDTH,HEIGHT,SETWINDOWPOSITIONSTYLE)
        val DestroyWindow          = rcall_I "DestroyWindow" SUCCESSSTATE (HWND)
        val FindWindow             = rcall_II "FindWindowA" HWND (STRING,STRING)
        val GetDesktopWindow       = rcall_O "GetDesktopWindow" HWND (VOID)
        val GetForegroundWindow    = rcall_O "GetForegroundWindow" HWND (VOID)
        val GetLastActivePopup     = rcall_I "GetLastActivePopup" HWND (HWND)
        val GetParent              = rcall_I "GetParent" HWND (HWND)
        val GetTopWindow           = rcall_I "GetTopWindow" HWND (HWND)
        val GetWindow              = rcall_II "GetWindow" HWND (HWND,GETWINDOWFLAG)
        val GetClientRect          = rcall_IW "GetClientRect" BOOL (HWND,RECTANGLE)
        val GetNextWindow          = rcall_II "GetNextWindow" HWND (HWND,GETWINDOWFLAG)
        val GetWindowTextLength    = rcall_I "GetWindowTextLength" INT (HWND)
        val IsChild                = rcall_II "IsChild" BOOL (HWND,HWND)
        val IsIconic               = rcall_I "IsIconic" BOOL (HWND)
        val IsWindow               = rcall_I "IsWindow" BOOL (HWND)
        val IsWindowVisible        = rcall_I "IsWindowVisible" BOOL (HWND)
        val IsZoomed               = rcall_I "IsZoomed" BOOL (HWND)
        val MoveWindow             = rcall_IIIIII "MoveWindow" SUCCESSSTATE (HWND,XCOORD,YCOORD,WIDTH,HEIGHT,BOOL)
        val OpenIcon               = rcall_I "OpenIcon" SUCCESSSTATE (HWND)
        val SetForegroundWindow    = rcall_I "SetForegroundWindow" SUCCESSSTATE (HWND)
        val SetParent              = rcall_II "SetParent" HWND (HWND,HWND)
        val SetWindowPos           = rcall_IIIIIII "SetWindowPos" SUCCESSSTATE
                                     (HWND,HWND,XCOORD,YCOORD,WIDTH,HEIGHT,SETWINDOWPOSITIONSTYLE)
        val SetWindowText          = rcall_II "SetWindowTextA" SUCCESSSTATE (HWND,STRING)
        val ShowOwnedPopups        = rcall_II "ShowOwnedPopups" SUCCESSSTATE (HWND,BOOL)
        val WindowFromPoint        = rcall_I "WindowFromPoint" HWND (POINT)
        val GetClientRect          = rcall_II "GetClientRect" HEIGHT (HWND,WIDTH)

        val GetWindowRect          = rcall_IW "GetWindowRect" BOOL (HWND,RECTANGLE)

        val MessageBox             = rcall_IIII "MessageBoxA" MESSAGEBOXIDENTIFIER (HWND,STRING,STRING,MESSAGEBOXVALUE) 
    end
end
