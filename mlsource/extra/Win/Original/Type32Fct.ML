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
  structure UpperCaseConversions :
  sig
  end

  type PaletteEntryFlag
  type MenuFlag
  type IconIdentifier
  type ColorAdjustmentFlag
  type IlluminantConstant

end;

signature importCInterfaceSig = 
sig
  type vol
  type Ctype
  type 'a Conversion 
  val mkConversion : (vol -> 'a) -> ('a -> vol) -> Ctype -> 'a Conversion
  val breakConversion : 'a Conversion -> (vol -> 'a) * ('a -> vol) * Ctype
  val toCint : int -> vol
  val STRUCT2 : 'a1 Conversion * 'a2 Conversion -> ('a1 * 'a2) Conversion
  val STRUCT4 : 'a1 Conversion * 'a2 Conversion * 'a3 Conversion * 'a4 Conversion
             -> ('a1 * 'a2 * 'a3 * 'a4) Conversion 
  val STRUCT6 : 'a1 Conversion * 'a2 Conversion * 'a3 Conversion * 'a4 Conversion
            * 'a5 Conversion * 'a6 Conversion
             -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) Conversion

  val voidStar : Ctype
  val FLOAT   : real Conversion
  val INT     : int Conversion
  val LONG    : int Conversion
  val BOOL    : bool Conversion

end;



functor Type32Fct ( structure CInterface : importCInterfaceSig
                    structure ConstType : importConstTypeSig 
                  ) =
struct
 
open CInterface
open ConstType 

datatype MLHACCEL     = MLHACCEL of vol (* Handle of an accelerator table *)
val HACCEL            = mkConversion (MLHACCEL) ((fn (MLHACCEL h) => h)) (voidStar)

datatype MLHANDLE     = MLHANDLE of vol (* Handle of an object *)
val HANDLE            = mkConversion (MLHANDLE) ((fn (MLHANDLE h) => h)) (voidStar)

datatype MLHBITMAP    = MLHBITMAP of vol (* Handle of a bitmap *)
val HBITMAP           = mkConversion (MLHBITMAP) ((fn (MLHBITMAP h) => h)) (voidStar)
 
datatype MLHBRUSH     = MLHBRUSH of vol (* Handle of a brush *)
val HBRUSH            = mkConversion (MLHBRUSH) ((fn (MLHBRUSH h) => h)) (voidStar)

datatype MLHCONV      = MLHCONV of vol (* Handle of a dynamic data exchange (DDE) conversation *)
val HCONV             = mkConversion (MLHCONV) ((fn (MLHCONV h) => h)) (voidStar)

datatype MLHCONVLIST  = MLHCONVLIST of vol (* Handle of a DDE conversation list *)
val HCONVLIST         = mkConversion (MLHCONVLIST) ((fn (MLHCONVLIST h) => h)) (voidStar)

datatype MLHCURSOR    = MLHCURSOR of vol (* Handle of a cursor *)
val HCURSOR           = mkConversion (MLHCURSOR) ((fn (MLHCURSOR h) => h)) (voidStar)

datatype MLHDC        = MLHDC of vol (* Handle of a device context (DC) *)
val HDC               = mkConversion (MLHDC) ((fn (MLHDC h) => h)) (voidStar)

datatype MLHDDEDATA   = MLHDDEDATA of vol (* Handle of DDE data *)
val HDDEDATA          = mkConversion (MLHDDEDATA) ((fn (MLHDDEDATA h) => h)) (voidStar)

datatype MLHDWP       = MLHDWP of vol (* Handle of a deferred window position structure *)
val HDWP              = mkConversion (MLHDWP) ((fn (MLHDWP h) => h)) (voidStar)

datatype MLHENHMETAFILE = MLHENHMETAFILE of vol (* Handle of an enhanced metafile *)
val HENHMETAFILE        = mkConversion (MLHENHMETAFILE) ((fn (MLHENHMETAFILE h) => h)) (voidStar)

datatype MLHFILE      = MLHFILE of vol (* Handle of a file *)
val HFILE             = mkConversion (MLHFILE) ((fn (MLHFILE h) => h)) (voidStar)

datatype MLHFONT      = MLHFONT of vol (* Handle of a font *)
val HFONT             = mkConversion (MLHFONT) ((fn (MLHFONT h) => h)) (voidStar)

datatype MLHGDIOBJ    = MLHGDIOBJ of vol (* Handle of a GDI object *)
val HGDIOBJ           = mkConversion (MLHGDIOBJ) ((fn (MLHGDIOBJ h) => h)) (voidStar)

datatype MLHGLOBAL    = MLHGLOBAL of vol (* Handle of a global memory block *)
val HGLOBAL           = mkConversion (MLHGLOBAL) ((fn (MLHGLOBAL h) => h)) (voidStar)

datatype MLHICON      = MLHICON of vol (* Handle of an icon *)
val HICON             = mkConversion (MLHICON) ((fn (MLHICON h) => h)) (voidStar)

datatype MLHINSTANCE  = MLHINSTANCE of vol (* Handle of an instance *)
val HINSTANCE         = mkConversion (MLHINSTANCE) ((fn (MLHINSTANCE h) => h)) (voidStar)

datatype MLHKEY       = MLHKEY of vol (* Handle of a registry key *)
val HKEY              = mkConversion (MLHKEY) ((fn (MLHKEY h) => h)) (voidStar)

datatype MLHLOCAL     = MLHLOCAL of vol (* Handle of a local memory block *)
val HLOCAL            = mkConversion (MLHLOCAL) ((fn (MLHLOCAL h) => h)) (voidStar)

datatype MLHMENU      = MLHMENU of vol (* Handle of a menu *)
val HMENU             = mkConversion (MLHMENU) ((fn (MLHMENU h) => h)) (voidStar)

datatype MLHMETAFILE  = MLHMETAFILE of vol (* Handle of a metafile *)
val HMETAFILE         = mkConversion (MLHMETAFILE) ((fn (MLHMETAFILE h) => h)) (voidStar)

datatype MLHPALETTE   = MLHPALETTE of vol (* Handle of a palette *)
val HPALETTE          = mkConversion (MLHPALETTE) ((fn (MLHPALETTE h) => h)) (voidStar)

datatype MLHPEN       = MLHPEN of vol (* Handle of a pen *)
val HPEN              = mkConversion (MLHPEN) ((fn (MLHPEN h) => h)) (voidStar)

datatype MLHRGN       = MLHRGN of vol (* Handle of a region *)
val HRGN              = mkConversion (MLHRGN) ((fn (MLHRGN h) => h)) (voidStar)

datatype MLHRSRC      = MLHRSRC of vol (* Handle of a resource *)
val HRSRC             = mkConversion (MLHRSRC) ((fn (MLHRSRC h) => h)) (voidStar)

datatype MLHSZ        = MLHSZ of vol (* Handle of a DDE string *)
val HSZ               = mkConversion (MLHSZ) ((fn (MLHSZ h) => h)) (voidStar)

datatype MLHWND       = MLHWND of vol (* Handle of a window *)
val HWND              = mkConversion (MLHWND) ((fn (MLHWND h) => h)) (voidStar)

datatype MLPAINT       = MLPAINT of vol (* Wraps PaintStructure vol *)
val PAINT              = mkConversion (MLPAINT) ((fn (MLPAINT h) => h)) (voidStar)


fun mkBrush  (MLHGDIOBJ v) = (MLHBRUSH v)
fun mkBitmap (MLHGDIOBJ v) = (MLHBITMAP v)
fun mkFont   (MLHGDIOBJ v) = (MLHFONT v)
fun mkPen    (MLHGDIOBJ v) = (MLHPEN v)
fun mkRegion (MLHGDIOBJ v) = (MLHRGN v)

(* PlacementOrder constants *)

val HWND_BROADCAST  = MLHWND (toCint (Macro.hex "0xffff"))
val HWND_DESKTOP    = MLHWND (toCint 0)
val HWND_TOP        = MLHWND (toCint 0)
val HWND_BOTTOM     = MLHWND (toCint 1)
val HWND_TOPMOST    = MLHWND (toCint ~1)
val HWND_NOTOPMOST  = MLHWND (toCint ~2)


datatype Color = Rgb of int * int *int

type ClassName = string
type WindowName = string
type Xcoord = int
type Ycoord = int
type Xoffset = int
type Yoffset = int
type Width = int
type Height = int
type Index = int
type SuccessState = bool
type DCinstance = int
type ForceBackground = bool
type ListBox = int
type StaticControl = int
datatype MenuItem = MenuID of int
type RWpermission = int
type DCInstance = int
type Fmf = int

val MENUITEM = INT : int Conversion
val XCOORD = INT : Xcoord Conversion
val YCOORD = INT: Ycoord Conversion
val XOFFSET = INT: Xoffset Conversion
val YOFFSET = INT: Yoffset Conversion
val WIDTH = INT: Width Conversion
val HEIGHT = INT: Height Conversion
val INDEX = INT: Index Conversion
val SUCCESSSTATE = BOOL: SuccessState Conversion
val RWPERMISSION = INT: RWpermission Conversion
val FORCEBACKGROUND = BOOL : ForceBackground Conversion
val LISTBOX = INT : ListBox Conversion
val STATICCONTROL = INT : StaticControl Conversion
val DCINSTANCE = INT : DCInstance Conversion
val FONTMAPPERFLAG = INT : Fmf Conversion


datatype Range = Range of {min:int,max:int} 

type SuccessState = bool

datatype KeyState = KeyUp | KeyDown  | NoKey

datatype ShiftState = Shift | Ctrl | Alt | Comb of ShiftState list 

datatype Wnd = W of MLHWND * int

datatype 'a Option = Some of 'a | None

datatype BitmapData = BitmapData of Color list list 


fun absConversion {abs,rep} C = 
let val (fromC,toC,Ctype) = breakConversion C   
in mkConversion (abs o fromC) (toC o rep) (Ctype)
end

abstype Size = S of int * int
with                    
 fun mkSize {width,height} = S (width,height)
 fun breakSize (S (width,height)) = {width=width,height=height}

 val SIZE = absConversion {abs=(fn x => S x),
                           rep=(fn S x => x)} (STRUCT2 (LONG,LONG))
end

abstype Rectangle = R of int * int * int * int
with
 fun mkRect {left,top,right,bottom} = R (left,top,right,bottom)
 fun breakRect (R (left,top,right,bottom)) = {left=left,top=top,right=right,bottom=bottom}

 val RECTANGLE = absConversion {abs=(fn x => R x),
                                rep=(fn R x => x)} (STRUCT4 (LONG,LONG,LONG,LONG))
end

abstype Point = P of int * int
with
 fun mkPoint {x,y} = P (x,y)
 fun breakPoint (P (x,y)) = {x=x,y=y}

 val POINT = absConversion {abs=(fn x => P x),
                            rep=(fn P x => x)} (STRUCT2 (LONG,LONG))
end

abstype XForm = X of real * real * real * real * real * real
with
 fun mkXForm {r11,r12,r21,r22,tx,ty} = X (r11,r12,r21,r22,tx,ty)
 fun breakXForm (X (r11,r12,r21,r22,tx,ty)) = {r11=r11,r12=r12,r21=r21,
                                               r22=r22,tx=tx,ty=ty}

 val XFORM = absConversion {abs=(fn x => X x),
                            rep=(fn X x => x)} 
                            (STRUCT6 (FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT))
end


datatype PaletteEntry = PE of {red:int,green:int,
                               blue:int,flag:PaletteEntryFlag}

datatype ColorAdjustment = CA of {flags:ColorAdjustmentFlag,
                                  illuminance:IlluminantConstant,
                                  Rgamma:int,
                                  Ggamma:int,
                                  Bgamma:int,
                                  referenceBlack:int,
                                  referenceWhite:int,
                                  contrast:int,
                                  brightness:int,
                                  colorfullness:int,
                                  Rgammatint:int }

datatype Fraction = Fraction of {num:int,denom:int}

datatype ABC = ABC of { Aspacing:int,
                        Bspacing:int,
                        Cspacing:int }

datatype ABCfloat = ABCfloat of { Aspacing:real,
                                  Bspacing:real,
                                  Cspacing:real }

datatype VirtualKeyData = VK of int list

datatype MenuItemData = MID of {option:MenuFlag,
                                id:MenuItem,
                                display:string}


type PaintData = MLHDC * bool * Rectangle

datatype 'a option = Some of 'a | None

datatype Id = Id of int
val nextfreeId = ref 0
fun newId () = (nextfreeId:= !nextfreeId +1; Id(!nextfreeId))
fun intOfId (Id n) = n
fun sameId (Id n, Id m) = n=m


end;


