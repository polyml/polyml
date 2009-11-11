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

signature importCInterfaceSig =
sig
  type sym
  type 'a Conversion
  type Ctype
  type vol
   
  val global   : sym -> 'a Conversion -> unit -> 'a
  val get_sym  : string -> string -> sym
  val volOfSym : sym -> vol

  val alloc        : int -> Ctype -> vol
  val address      : vol -> vol
  val deref        : vol -> vol
  val offset       : int -> Ctype -> vol -> vol
  val assign       : Ctype -> vol -> vol -> unit

  val Cchar       : Ctype
  val Cfloat      : Ctype
  val Cint        : Ctype
  val Clong       : Ctype
  val Cshort      : Ctype
  val Cpointer    : Ctype -> Ctype
  val Cstruct     : Ctype list -> Ctype
  val Cfunction   : Ctype list * Ctype -> Ctype
  val Cvoid       : Ctype

  val voidStar : Ctype

  val toCstring    : string -> vol
  val toCchar      : string -> vol
  val toCfloat     : real -> vol
  val toCint       : int -> vol
  val toClong      : int -> vol
  val toCshort     : int -> vol

  val fromCstring  : vol -> string
  val fromCchar    : vol -> string
  val fromCfloat   : vol -> real
  val fromCint     : vol -> int
  val fromClong    : vol -> int
  val fromCshort   : vol -> int

  val call_sym     : sym -> (Ctype * vol) list -> Ctype -> vol
  val break_struct  : Ctype list -> vol -> vol list
  val make_struct   : (Ctype * vol) list -> vol

  val CHAR  : string Conversion
  val DOUBLE  : real Conversion
  val FLOAT   : real Conversion
  val INT     : int Conversion
  val LONG    : int Conversion
  val SHORT   : int Conversion
  val STRING  : string Conversion
  val VOID    : unit Conversion
  val BOOL    : bool Conversion
  val POINTER : vol Conversion

    val call0   : sym -> unit -> 'a Conversion -> unit -> 'a
    val call1   : sym -> 'a Conversion -> 'b Conversion -> 'a -> 'b
    val call2   : sym -> 'a Conversion * 'b Conversion -> 'c Conversion -> 'a * 'b -> 'c
    
   val call3 : sym ->
      'a Conversion
    * 'b Conversion
    * 'c Conversion -> 'd Conversion
    -> 'a * 'b * 'c -> 'd
    
    val call4 : sym ->
      'a Conversion
    * 'b Conversion
    * 'c Conversion
    * 'd Conversion -> 'e Conversion
    -> 'a * 'b * 'c * 'd -> 'e

    val call5 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion -> 'f Conversion
    -> 'a * 'b * 'c * 'd * 'e -> 'f

    val call6 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion -> 'g Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f -> 'g

    val call7 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion 
    * 'g Conversion -> 'h Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f * 'g -> 'h

    val call8 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion 
    * 'g Conversion 
    * 'h Conversion -> 'i Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> 'i

    val call9 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion 
    * 'g Conversion 
    * 'h Conversion 
    * 'i Conversion -> 'j Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i -> 'j

end;

signature importMacroSig =
sig
  val GetRValue : int -> int
  val GetGValue : int -> int
  val GetBValue : int -> int
  val PALETTERGB : int * int * int -> int
  val BYTE2LONG  : int * int * int * int -> int
  val LONG2BYTE  : int -> int * int * int * int
  val MAKELONG : int * int -> int
  val LOWORD : int -> int
  val HIWORD : int -> int
  val LOBYTE : int -> int
  val HIBYTE : int -> int
  val natAnd : int * int -> int

end;

signature importConstTypeSig =
sig
   type 'a encoding
   type 'a Conversion
   val absE : 'a encoding -> int -> 'a
   val repE : 'a encoding -> 'a -> int

   type WindowStyleExtended
   type WindowStyle
   type ClassStyle
   type ButtonStyle
   type ComboBoxStyle
   type EditStyle
   type ListBoxStyle
   type ScrollBarStyle
   type StaticStyle
   type FloodFillMode
   type BrushStyle
   type HatchStyle
   type ResultRegion
   type PaletteEntryFlag
   type PointType
   type PenStyle
   type ExtendedTextOut
   type PolyFillMode
   type MenuFlag
   type ScrollBarType
   type IconIdentifier
   type CursorIdentifier
   type WindowPlacementFlag
   type ShowWindowMode

   val WindowPlacementFlagE : WindowPlacementFlag encoding
   val ShowWindowModeE : ShowWindowMode encoding
   val WindowStyleExtendedE : WindowStyleExtended encoding
   val WindowStyleE : WindowStyle encoding
   val ClassStyleE : ClassStyle encoding
   val ButtonStyleE : ButtonStyle encoding
   val ComboBoxStyleE : ComboBoxStyle encoding
   val EditStyleE : EditStyle encoding  
   val ListBoxStyleE : ListBoxStyle encoding
   val ScrollBarStyleE : ScrollBarStyle encoding
   val StaticStyleE : StaticStyle encoding
   val MenuFlagE : MenuFlag encoding
   val IconIdentifierE : IconIdentifier encoding
   val CursorIdentifierE : CursorIdentifier encoding

   structure UpperCaseConversions :
    sig 
     val ICONIDENTIFIER : IconIdentifier Conversion
     val CURSORIDENTIFIER : CursorIdentifier Conversion
     val SCROLLBARTYPE : ScrollBarType Conversion
     val MENUFLAG : MenuFlag Conversion
     val POLYFILLMODE :PolyFillMode Conversion
     val FLOODFILLMODE : FloodFillMode Conversion
     val PENSTYLE : PenStyle Conversion
     val EXTENDEDTEXTOUT : ExtendedTextOut Conversion
    end

   val BrushStyleE : BrushStyle encoding
   val HatchStyleE : HatchStyle encoding
   val ResultRegionE : ResultRegion encoding
   val PaletteEntryFlagE : PaletteEntryFlag encoding 
   val PointTypeE : PointType encoding

end;

signature importTypeSig =
sig
  type vol
  type 'a Conversion
  type PaletteEntryFlag
  type MenuFlag
  datatype MenuItem = MenuID of int

  datatype MLHANDLE = MLHANDLE of vol
  val HANDLE : MLHANDLE Conversion
  datatype MLHWND = MLHWND of vol
  val HWND : MLHWND Conversion
  datatype MLHDC =  MLHDC of vol
  val HDC : MLHDC Conversion
  datatype MLHMENU  = MLHMENU of vol
  val HMENU : MLHMENU Conversion
  datatype MLHRGN = MLHRGN of vol
  val HRGN : MLHRGN Conversion
  datatype MLHFONT = MLHFONT of vol
  val HFONT : MLHFONT Conversion
  datatype MLHICON = MLHICON of vol
  val HICON : MLHICON Conversion
  datatype MLHCURSOR = MLHCURSOR of vol
  val HCURSOR : MLHCURSOR Conversion
  datatype MLHBRUSH = MLHBRUSH of vol
  val HBRUSH : MLHBRUSH Conversion
  datatype MLHPALETTE = MLHPALETTE of vol
  val HPALETTE : MLHPALETTE Conversion
  datatype MLHPEN = MLHPEN of vol
  val HPEN : MLHPEN Conversion
  datatype MLPAINT = MLPAINT of vol
  val PAINT : MLPAINT Conversion

  datatype Range = Range of {min:int,max:int}      
  datatype Color = Rgb of int * int *int

  type Xcoord 
  type Ycoord
  type Height
  type Width

  val XCOORD : Xcoord Conversion
  val YCOORD : Ycoord Conversion
  val HEIGHT : Height Conversion
  val WIDTH  : Width Conversion

  type Point
  type Rectangle
  type Size
  type XForm

  val mkPoint : {x:int,y:int} -> Point
  val breakPoint : Point -> {x:int,y:int}
  val mkSize : {width:int,height:int} -> Size
  val breakSize : Size -> {width:int,height:int}

  val mkRect : {left:int,top:int,right:int,bottom:int} -> Rectangle
  val breakRect : Rectangle -> {left:int,top:int,right:int,bottom:int}

  val breakXForm : XForm -> {r11:real,r12:real,r21:real,
                             r22:real,tx:real,ty:real}

  datatype PaletteEntry = PE of {red:int,green:int,
                                 blue:int,flag:PaletteEntryFlag}

  datatype Fraction = Fraction of {num:int,denom:int}

  datatype ABC = ABC of { Aspacing:int,
                          Bspacing:int,
                          Cspacing:int }

  datatype ABCfloat = ABCfloat of { Aspacing:real,
                                     Bspacing:real,
                                     Cspacing:real }

 datatype ShiftState = Shift | Ctrl | Alt | Comb of ShiftState list
 datatype VirtualKeyData = VK of int list

 datatype MenuItemData = MID of {option:MenuFlag,
                                 id:MenuItem,
                                 display:string}

end;

signature importDirSig =
sig
   val wpolyname   : string 
   val gdilibname  : string
   val userlibname : string
end;

functor UserFn2Fct ( structure Dir : importDirSig
                     structure CInterface : importCInterfaceSig
                     structure Macro      : importMacroSig
                     structure ConstType  : importConstTypeSig
                     structure Type       : importTypeSig

                     sharing type CInterface.Conversion =
                                  Type.Conversion =
                                  ConstType.Conversion
                     sharing type CInterface.vol =
                                  Type.vol
                     sharing type Type.PaletteEntryFlag =
                                  ConstType.PaletteEntryFlag
                     sharing type Type.MenuFlag =
                                  ConstType.MenuFlag              

                   ) = 

struct 
 
open CInterface
open Macro
open Type
open ConstType
open UpperCaseConversions
open Dir

local
val Executable = wpolyname
fun Getv (v,conv) = global (get_sym Executable v) conv ()
val getuser = get_sym userlibname
val getgdi = get_sym gdilibname

exception WrongNumberOfVolatiles

fun break_struct2 types v =
let val vols = break_struct types v
in case vols of
   [v1,v2] => (v1,v2)
 | _       =>  raise WrongNumberOfVolatiles   
end;

fun break_struct3 types v =
let val vols = break_struct types v
in case vols of
   [v1,v2,v3] => (v1,v2,v3)
 | _       =>  raise WrongNumberOfVolatiles   
end;

fun break_struct11 types v =
let val vols = break_struct types v
in case vols of
   [v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11] => 
          (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11)
 | _       =>  raise WrongNumberOfVolatiles   
end;


in

(* WINDOW CREATION *)

(* 
   mkWindow : WindowStyleExtended *               
              ClassStyle * 
              MLHICON * 
              MLHCURSOR * 
              MLHBRUSH *           
              WindowName * 
              WindowStyle * 
              Xcoord * Ycoord * Width * Height * 
              MLHWND * 
              MLHMENU -> MLHWND 
*) 

fun mkWindow (exstyle,cstyle,i,c,b,name,style,x,y,w,h,parent,menu) =
let 

   val UserWproc = volOfSym (get_sym Executable "UserWindowClassHandler") 
   val instance = Getv ("hinst",HANDLE)

   val temporaryclass_name = "mywonderfulclass"

   val myclass = make_struct 
            [ (Cint, toCint (repE ClassStyleE cstyle)),
              (Cpointer (Cfunction ([voidStar,Cint,Cint,Clong],Clong)),UserWproc),
              (Cint, toCint 0),
              (Cint, toCint 0),
              (voidStar, (fn (MLHANDLE v) => v) instance),
              (voidStar, (fn (MLHICON v) => v) i),
              (voidStar, (fn (MLHCURSOR v) => v) c),   
              (voidStar, (fn (MLHBRUSH v) => v) b),
              (Cpointer Cchar,toCint 0),
              (Cpointer Cchar, toCstring temporaryclass_name) ]

   val success = call1 (getuser "RegisterClassA")  (POINTER) (BOOL) (address  myclass)

in
   MLHWND 
    (
     call_sym (getuser "CreateWindowExA") 
     [(Clong,toClong (repE WindowStyleExtendedE exstyle) ), 
      (Cpointer Cchar,toCstring temporaryclass_name),
      (Cpointer Cchar,toCstring name),
      (Clong,toClong (repE WindowStyleE style)),
      (Cint,toCint x),
      (Cint,toCint y),
      (Cint,toCint w),
      (Cint,toCint h),
      (voidStar,(fn (MLHWND v) => v) parent),
      (voidStar,(fn (MLHMENU v) => v) menu), 
      (voidStar,(fn (MLHANDLE v) => v) instance),
      (voidStar,toCint 0)]  voidStar 
    )
end 

(* 
    mkButton : string * 
               ButtonStyle * 
               Xcoord * Ycoord * Width * Height * int * 
               HWND -> HWND 
*)

fun mkButton (name,style,x,y,w,h,i,parent) =
let
   val instance = Getv ("hinst",HANDLE)
in
   MLHWND 
    (
     call_sym (getuser "CreateWindowExA") 
     [(Clong,toClong 0 ), 
      (Cpointer Cchar,toCstring "BUTTON"),
      (Cpointer Cchar,toCstring name),
      (Clong,toClong (repE ButtonStyleE style)),
      (Cint,toCint x),
      (Cint,toCint y),
      (Cint,toCint w),
      (Cint,toCint h),
      (voidStar,(fn (MLHWND v) => v) parent),
      (voidStar,toCint i),  (* i is the child control identifier *)  
      (voidStar,(fn (MLHANDLE v) => v) instance),
      (voidStar,toCint 0)]  voidStar 
    )

end

(*
   mkComboBox : string * 
                ComboBoxStyle * 
                Xcoord * Ycoord * Width * Height * int * 
                HWND -> HWND
*)

fun mkComboBox (name,style,x,y,w,h,i,parent) =
let
   val instance = Getv ("hinst",HANDLE)
in
   MLHWND 
    (
     call_sym (getuser "CreateWindowExA") 
     [(Clong,toClong 0 ), 
      (Cpointer Cchar,toCstring "COMBOBOX"),
      (Cpointer Cchar,toCstring name),
      (Clong,toClong (repE ComboBoxStyleE style)),
      (Cint,toCint x),
      (Cint,toCint y),
      (Cint,toCint w),
      (Cint,toCint h),
      (voidStar,(fn (MLHWND v) => v) parent),
      (voidStar,toCint i), (* i is the child control identifier *)
      (voidStar,(fn (MLHANDLE v) => v) instance),
      (voidStar,toCint 0)]  voidStar 
    )
end

(*
   mkEdit : string * 
            EditStyle * 
            Xcoord * Ycoord * Width * Height * int * 
            HWND -> HWND
*)

fun mkEdit (name,style,x,y,w,h,i,parent) =
let
   val instance = Getv ("hinst",HANDLE)
in
   MLHWND 
    (
     call_sym (getuser "CreateWindowExA") 
     [(Clong,toClong 0 ), 
      (Cpointer Cchar,toCstring "EDIT"),
      (Cpointer Cchar,toCstring name),
      (Clong,toClong (repE EditStyleE style)),
      (Cint,toCint x),
      (Cint,toCint y),
      (Cint,toCint w),
      (Cint,toCint h),
      (voidStar,(fn (MLHWND v) => v) parent),
      (voidStar,toCint i), (* i is the child control identifier *) 
      (voidStar,(fn (MLHANDLE v) => v) instance),
      (voidStar,toCint 0)]  voidStar 
    )
end

(*
   mkListBox : string * 
               ListBoxStyle * 
               Xcoord * Ycoord * Width * Height * int * 
               HWND -> HWND
*)

fun mkListBox (name,style,x,y,w,h,i,parent) =
let
   val instance = Getv ("hinst",HANDLE)
in
   MLHWND 
    (
     call_sym (getuser "CreateWindowExA") 
     [(Clong,toClong 0 ), 
      (Cpointer Cchar,toCstring "LISTBOX"),
      (Cpointer Cchar,toCstring name),
      (Clong,toClong (repE ListBoxStyleE style)),
      (Cint,toCint x),
      (Cint,toCint y),
      (Cint,toCint w),
      (Cint,toCint h),
      (voidStar,(fn (MLHWND v) => v) parent),
      (voidStar,toCint i), (* i is the child control identifier *) 
      (voidStar,(fn (MLHANDLE v) => v) instance),
      (voidStar,toCint 0)]  voidStar 
    )
end


(*
   mkScrollBar : string * 
                 ScrollBarStyle * 
                 Xcoord * Ycoord * Width * Height * int * 
                 HWND -> HWND
*)

fun mkScrollBar (name,style,x,y,w,h,i,parent) =
let
   val instance = Getv ("hinst",HANDLE)
in
   MLHWND 
    (
     call_sym (getuser "CreateWindowExA") 
     [(Clong,toClong 0 ), 
      (Cpointer Cchar,toCstring "SCROLLBAR"),
      (Cpointer Cchar,toCstring name),
      (Clong,toClong (repE ScrollBarStyleE style)),
      (Cint,toCint x),
      (Cint,toCint y),
      (Cint,toCint w),
      (Cint,toCint h),
      (voidStar,(fn (MLHWND v) => v) parent),
      (voidStar,toCint i), (* i is the child control identifier *) 
      (voidStar,(fn (MLHANDLE v) => v) instance),
      (voidStar,toCint 0)]  voidStar 
    )
end

(*
   mkStatic : string * 
              StaticStyle * 
              Xcoord * Ycoord * Width * Height * 
              HWND -> HWND
*)
 
fun mkStatic (name,style,x,y,w,h,parent) =
let
   val instance = Getv ("hinst",HANDLE)
in
   MLHWND 
    (
     call_sym (getuser "CreateWindowExA") 
     [(Clong,toClong 0 ), 
      (Cpointer Cchar,toCstring "STATIC"),
      (Cpointer Cchar,toCstring name),
      (Clong,toClong (repE StaticStyleE style)),
      (Cint,toCint x),
      (Cint,toCint y),
      (Cint,toCint w),
      (Cint,toCint h),
      (voidStar,(fn (MLHWND v) => v) parent),
      (voidStar,toCint 0), 
      (voidStar,(fn (MLHANDLE v) => v) instance),
      (voidStar,toCint 0)]  voidStar 
    )
end


(* BITMAPS *)

fun ExtFloodFill (h,x,y,Rgb(r,g,b),f) = 
     call5 (getgdi "ExtFloodFill") 
           (HDC,XCOORD,YCOORD,LONG,FLOODFILLMODE) (BOOL) 
           (h,x,y,PALETTERGB(r,g,b),f)

fun GetPixel (h,x,y) = 
let val r = call3 (getgdi "GetPixel")
                  (HDC,XCOORD,YCOORD) (LONG)
                  (h,x,y)
in Rgb(GetRValue r,GetGValue r,GetBValue r)
end

fun SetPixel (h,x,y,Rgb(r,g,b)) = 
     call4 (getgdi "SetPixel") 
           (HDC,XCOORD,YCOORD,LONG) (BOOL) 
           (h,x,y,PALETTERGB(r,g,b))

(* BRUSHES *)

fun CreateBrushIndirect (br,Rgb(r,g,b),h) = 
let val logbrush = make_struct
            [ (Clong,toClong (repE BrushStyleE br)),
              (Clong,toClong (PALETTERGB(r,g,b))),
              (Clong,toClong (repE HatchStyleE h))]
in call1 (getuser "CreateBrushIndirect") 
         (POINTER) (HBRUSH) (address logbrush)
end

fun CreateHatchBrush (h,Rgb(r,g,b)) =
      call2 (getgdi "CreateHatchBrush")
            (HBRUSH,LONG) (LONG)
            (h,PALETTERGB(r,g,b))

fun CreateSolidBrush (Rgb(r,g,b)) =
      call1 (getgdi "CreateSolidBrush")
            (LONG) (HBRUSH)
            (PALETTERGB(r,g,b)) 

(* CLIPPING *)

fun ExcludeClipRect (h,rect) =
let
  val  {left,top,right,bottom} = breakRect rect
in
      absE ResultRegionE 
       ( call5 (getgdi "ExcludeClipRect")
               (HDC,LONG,LONG,LONG,LONG) (INT)
               (h,left,top,right,bottom) )
end
 
fun IntersectClipRect (h,rect) =
let
  val  {left,top,right,bottom} = breakRect rect
in
      absE ResultRegionE 
       ( call5 (getgdi "IntersectClipRect")
               (HDC,LONG,LONG,LONG,LONG) (INT)
               (h,left,top,right,bottom) )
end

(* COLORS *)

fun GetNearestColor (h,Rgb(r,g,b)) =
let val rgb = call2 (getgdi "GetNearestColor")
                    (HDC,LONG) (LONG)
                    (h,PALETTERGB(r,g,b)) 
in Rgb(GetRValue rgb,GetGValue rgb,GetBValue rgb)
end 

fun GetNearestPaletteIndex (h,Rgb(r,g,b)) = 
     call2 (getgdi "GetNearestPaletteIndex")
           (HPALETTE,LONG) (INT)
           (h,PALETTERGB(r,g,b))

fun AnimatePalette (h,start,pl) =
let val count = List.length pl
    val pal = alloc count Clong

    fun pl2a v n [] = () 
    |   pl2a v n (PE {red=r,green=g,blue=b,flag=f} :: rest) = 
    let
       val item = toClong (BYTE2LONG (r,g,b,repE PaletteEntryFlagE f))
    in
      ( assign Clong (offset n Clong v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a pal 0 pl

in call4 (getgdi "AnimatePalette")
         (HPALETTE,INT,INT,POINTER) (BOOL)
         (h,start,count,address pal)
end 

fun CreatePalette (pl) =
let val count = List.length pl
    
    fun pl2a v n [] = () 
    |   pl2a v n (PE {red=r,green=g,blue=b,flag=f} :: rest) = 
    let
       val item = toClong (BYTE2LONG (r,g,b,repE PaletteEntryFlagE f))
    in
      ( assign Clong (offset n Clong v) item ;
        pl2a v (n+1) rest ) 
    end

    val logpal = alloc (count+1) Clong

    val u = assign Cshort (offset 1 Cshort logpal) (toCshort count)
    val u = pl2a (offset 1 Clong logpal) 0 pl
in
   call1 (getgdi "CreatePalette")
         (POINTER) (HPALETTE)
         (address logpal)
end 

fun GetPaletteEntries (h,start,no) = 
let
  val palarr = alloc no Clong

  val res = call4 (getgdi "GetPaletteEntries")
            (HPALETTE,INT,INT,POINTER) (INT)
            (h,start,no,address palarr)

  fun v2l v 0 = []
  |   v2l v i = 
  let val (r,g,b,f) = LONG2BYTE (fromClong (offset (i-1) Clong v))
  in (v2l v (i-1)) @ [PE {red=r,green=g,blue=b,flag=absE PaletteEntryFlagE f} ] 
  end
in
   (res,v2l palarr no)
end 

fun GetSystemPaletteEntries (h,start,no) = 
let
  val palarr = alloc no Clong

  val res = call4 (getgdi "GetSystemPaletteEntries")
            (HPALETTE,INT,INT,POINTER) (INT)
            (h,start,no,address palarr)

  fun v2l v 0 = []
  |   v2l v i = 
  let val (r,g,b,f) = LONG2BYTE (fromClong (offset (i-1) Clong v))
  in (v2l v (i-1)) @ [PE {red=r,green=g,blue=b,flag=absE PaletteEntryFlagE f} ] 
  end
in
   (res,v2l palarr no)
end 
 
fun SetPaletteEntries (h,start,pl) =
let val count = List.length pl
    val pal = alloc count Clong

    fun pl2a v n [] = () 
    |   pl2a v n (PE {red=r,green=g,blue=b,flag=f} :: rest) = 
    let
       val item = toClong (BYTE2LONG (r,g,b,repE PaletteEntryFlagE f))
    in
      ( assign Clong (offset n Clong v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a pal 0 pl

in call4 (getgdi "SetPaletteEntries")
         (HPALETTE,INT,INT,POINTER) (BOOL)
         (h,start,count,address pal)
end

(* COORDINATE SPACES AND TRANSFORMATIONS *)

fun ScaleViewportExtEx (h,Fraction{num=n1,denom=d1},Fraction{num=n2,denom=d2}) =
let val sv = alloc 1 (Cstruct [Clong,Clong])
    val res = call6 (getgdi "ScaleViewportExtEx")
                    (HWND,INT,INT,INT,INT,POINTER) (BOOL)
                    (h,n1,d1,n2,d2,address sv) 

    val (wv,hv) = break_struct2 [Clong,Clong] sv
in
   mkSize {width=fromClong wv, height=fromClong hv}
end

fun ScaleWindowExtEx (h,Fraction{num=n1,denom=d1},Fraction{num=n2,denom=d2}) =
let val sv = alloc 1 (Cstruct [Clong,Clong])
    val res = call6 (getgdi "ScaleWindowExtEx")
                    (HWND,INT,INT,INT,INT,POINTER) (BOOL)
                    (h,n1,d1,n2,d2,address sv) 

    val (wv,hv) = break_struct2 [Clong,Clong] sv
in
   mkSize {width=fromClong wv, height=fromClong hv}
end

fun DPtoLP (h,pts) = 
let val count = List.length pts

    val ptarr = alloc count (Cstruct [Clong,Clong])

    fun pl2a v n [] = () 
    |   pl2a v n (p :: rest) = 
    let val {x,y} = breakPoint p
        val item = make_struct [(Clong,toClong x),
                                (Clong,toClong y)] 
    in
      ( assign (Cstruct [Clong,Clong]) (offset n (Cstruct [Clong,Clong]) v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a ptarr 0 pts
   
    val res = call3 (getgdi "DPtoLP")
              (HDC,POINTER,INT) (BOOL)
              (h, address ptarr,count) 

    fun v2l v 0 = []
    |   v2l v i = 
    let val vp = offset (i-1) (Cstruct [Clong,Clong]) v
        val (vx,vy) = break_struct2 [Clong,Clong] vp
    in (v2l v (i-1)) @ [(mkPoint {x=fromClong vx,y=fromClong vy})] 
    end
in
  (res,v2l ptarr count)
end

fun LPtoDP (h,pts) = 
let val count = List.length pts

    val ptarr = alloc count (Cstruct [Clong,Clong])

    fun pl2a v n [] = () 
    |   pl2a v n (p :: rest) = 
    let val {x,y} = breakPoint p
        val item = make_struct [(Clong,toClong x),
                                (Clong,toClong y)] 
    in
      ( assign (Cstruct [Clong,Clong]) (offset n (Cstruct [Clong,Clong]) v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a ptarr 0 pts
   
    val res = call3 (getgdi "LPtoDP")
              (HDC,POINTER,INT) (BOOL)
              (h, address ptarr,count) 

    fun v2l v 0 = []
    |   v2l v i = 
    let val vp = offset (i-1) (Cstruct [Clong,Clong]) v
        val (vx,vy) = break_struct2 [Clong,Clong] vp
    in (v2l v (i-1)) @ [mkPoint {x=fromClong vx,y=fromClong vy} ] 
    end
in
  (res,v2l ptarr count)
end
 
fun MapWindowPoints (h1,h2,pts) = 
let
    val count = List.length pts

    val ptarr = alloc count (Cstruct [Clong,Clong])

    fun pl2a v n [] = () 
    |   pl2a v n (p :: rest) = 
    let val {x,y} = breakPoint p
        val item = make_struct [(Clong,toClong x),
                                (Clong,toClong y)] 
    in
      ( assign (Cstruct [Clong,Clong]) (offset n (Cstruct [Clong,Clong]) v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a ptarr 0 pts

    val res = call4 (getuser "MapWindowPoints")
                    (HWND,HWND,POINTER,INT) (INT)
                    (h1,h2,address ptarr,count)

    fun v2l v 0 = []
    |   v2l v i = 
    let val vp = offset (i-1) (Cstruct [Clong,Clong]) v
        val (vx,vy) = break_struct2 [Clong,Clong] vp
    in (v2l v (i-1)) @ [mkPoint {x=fromClong vx,y=fromClong vy} ] 
    end

in
  ((LOWORD res,HIWORD res),v2l ptarr count)
end


(* DEVICE CONTEXTS *)

(* FILLED SHAPES *)

fun Chord (h,rect,p1,p2) =
let
  val {left,top,right,bottom} = breakRect rect
  val {x=x1,y=y1} = breakPoint p1
  val {x=x2,y=y2} = breakPoint p2
in
   call9 (getgdi "Chord")
         (HDC,LONG,LONG,LONG,LONG,LONG,LONG,LONG,LONG) (BOOL)
         (h,left,top,right,bottom,x1,y1,x2,y2)
end

fun Ellipse (h,rect) =
let
  val  {left,top,right,bottom} = breakRect rect
in 
    call5 (getgdi "Ellipse")
         (HDC,LONG,LONG,LONG,LONG) (BOOL)
         (h,left,top,right,bottom)
end

fun Pie (h,rect,p1,p2) =
let
  val  {left,top,right,bottom} = breakRect rect
  val {x=x1,y=y1} = breakPoint p1
  val {x=x2,y=y2} = breakPoint p2
in 
   call9 (getgdi "Pie")
         (HDC,LONG,LONG,LONG,LONG,LONG,LONG,LONG,LONG) (BOOL)
         (h,left,top,right,bottom,x1,y1,x2,y2)
end

fun DrawRectangle(h,rect) =
let
  val {left,top,right,bottom} = breakRect rect
in 
    call5 (getgdi "Rectangle")
          (HDC,LONG,LONG,LONG,LONG) (BOOL)
          (h,left,top,right,bottom)
end

fun DrawRoundRect(h,rect,w,ht) =
let
  val  {left,top,right,bottom} = breakRect rect
in 
    call7 (getgdi "RoundRect")
          (HDC,LONG,LONG,LONG,LONG,WIDTH,HEIGHT) (BOOL)
          (h,left,top,right,bottom,w,ht)
end

fun Polygon (h,pts) = 
let val count = List.length pts

    val ptarr = alloc count (Cstruct [Clong,Clong])

    fun pl2a v n [] = () 
    |   pl2a v n (p :: rest) = 
    let val {x,y} = breakPoint p
        val item = make_struct [(Clong,toClong x),
                                (Clong,toClong y)] 
    in
      ( assign (Cstruct [Clong,Clong]) (offset n (Cstruct [Clong,Clong]) v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a ptarr 0 pts

in call3 (getgdi "Polygon")
          (HDC,POINTER,INT) (BOOL)
          (h,address ptarr,count)
end

(* FONTS AND TEXT *) 

fun GetTextColor h = 
let val rgb = call1 (getgdi "GetTextColor") (HDC) (LONG) h
in Rgb (GetRValue rgb,GetGValue rgb,GetBValue rgb)
end

fun SetTextColor (h,Rgb(r,g,b)) =
let val rgb = call2 (getgdi "SetTextColor") (HDC,LONG) (LONG) (h,PALETTERGB(r,g,b))
in Rgb (GetRValue rgb,GetGValue rgb,GetBValue rgb)
end 

fun ExtTextOut (h,p,option,rect1,text,gapl) =
let val  {left,top,right,bottom} = breakRect rect1
    val rect = alloc 4 Clong 
    val u = (assign Clong (offset 0 Clong rect) (toClong left);
             assign Clong (offset 1 Clong rect) (toClong top);
             assign Clong (offset 2 Clong rect) (toClong right);
             assign Clong (offset 3 Clong rect) (toClong bottom) )

    val slen = String.size text

    val gapno = List.length gapl  
    val gaps = alloc gapno Cint

    fun pl2a v n [] = () 
    |   pl2a v n (x :: rest) = 
    let val item = toCint x 
    in ( assign Cint (offset n Cint v) item ;
        pl2a v (n+1) rest ) 
    end
    val u = pl2a gaps 0 gapl

    val {x,y} = breakPoint p 

in call8 (getgdi "ExtTextOutA")
         (HDC,LONG,LONG,EXTENDEDTEXTOUT,POINTER,STRING,INT,POINTER) (BOOL)
         (h,x,y,option,address rect,text,slen,address gaps)
end

fun GetCharABCWidths (h,c1,c2) = 
let val count = c2-c1
    val abcarr = alloc count (Cstruct [Cint,Cint,Cint])

    val res = call4 (getgdi "GetCharABCWidthsA")
                    (HDC,INT,INT,POINTER) (BOOL)
                    (h,c1,c2,address abcarr)

    fun v2l v 0 = []
    |   v2l v i = 
    let val vp = offset (i-1) (Cstruct [Cint,Cint,Cint]) v
        val (va,vb,vc) = break_struct3 [Cint,Cint,Cint] vp
    in (v2l v (i-1)) @ [ ABC { Aspacing = fromClong va,
                               Bspacing = fromClong vb,
                               Cspacing = fromClong vc } ] 
    end

in v2l abcarr count
end

fun GetCharABCWidthsFloat (h,c1,c2) = 
let val count = c2-c1
    val abcarr = alloc count (Cstruct [Cfloat,Cfloat,Cfloat])

    val res = call4 (getgdi "GetCharABCWidthsFloatA")
                    (HDC,INT,INT,POINTER) (BOOL)
                    (h,c1,c2,address abcarr)

    fun v2l v 0 = []
    |   v2l v i = 
    let val vp = offset (i-1) (Cstruct [Cfloat,Cfloat,Cfloat]) v
        val (va,vb,vc) = break_struct3 [Cfloat,Cfloat,Cfloat] vp
    in (v2l v (i-1)) @ [ ABCfloat { Aspacing = fromCfloat va,
                                    Bspacing = fromCfloat vb,
                                    Cspacing = fromCfloat vc } ] 
    end

in v2l abcarr count
end

fun GetCharWidth32 (h,c1,c2) = 
let val count = c2-c1
    val abcarr = alloc count Cint

    val res = call4 (getgdi "GetCharWidth32A")
                    (HDC,INT,INT,POINTER) (BOOL)
                    (h,c1,c2,address abcarr)

    fun v2l v 0 = []
    |   v2l v i = 
    let val vp = offset (i-1) Cint v
    in (v2l v (i-1)) @ [fromCint vp ] 
    end

in v2l abcarr count
end

fun GetCharWidthFloat (h,c1,c2) = 
let val count = c2-c1
    val abcarr = alloc count Cfloat

    val res = call4 (getgdi "GetCharWidthFloatA")
                    (HDC,INT,INT,POINTER) (BOOL)
                    (h,c1,c2,address abcarr)

    fun v2l v 0 = []
    |   v2l v i = 
    let val vp = offset (i-1) Cfloat v
    in (v2l v (i-1)) @ [fromCfloat vp ] 
    end

in v2l abcarr count
end

fun GetTextExtentPoint32 (h,s) = 
let val sz = alloc 1 (Cstruct [Clong,Clong])
    val slen = String.size s
    val res = call4 (getgdi "GetTextExtentPoint32A")
                    (HDC,STRING,INT,POINTER) (BOOL)
                    (h,s,slen,address sz)

    val (x,y) = break_struct2 [Clong,Clong] sz
in
   mkSize {width=fromClong x, height=fromClong y}
end

fun TextOut (h,p,s) = 
let val len = String.size s
    val {x,y} = breakPoint p
in
   call5 (getgdi "TextOut")
         (HDC,INT,INT,STRING,INT) (BOOL)
         (h,x,y,s,len)
end  

(* LINES AND CURVES *)

fun LineTo (h,p) =
let val {x,y} = breakPoint p
in call3 (getgdi "LineTo") (HDC,INT,INT) (BOOL) (h,x,y)
end

fun PolyBezier (h,pts) =
let val count = List.length pts
    val ptarr = alloc count (Cstruct [Clong,Clong])

    fun pl2a v n [] = () 
    |   pl2a v n (p :: rest) = 
    let val {x,y} = breakPoint p
        val item = make_struct [(Clong,toClong x),
                                (Clong,toClong y)] 
    in
      ( assign (Cstruct [Clong,Clong]) (offset n (Cstruct [Clong,Clong]) v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a ptarr 0 pts
in
   call3 (getgdi "PolyBezier")
         (HDC,POINTER,INT) (INT)
         (h,address ptarr,count)
end  

fun PolyBezierTo (h,pts) =
let val count = List.length pts
    val ptarr = alloc count (Cstruct [Clong,Clong])

    fun pl2a v n [] = () 
    |   pl2a v n (p :: rest) = 
    let val {x,y} = breakPoint p
        val item = make_struct [(Clong,toClong x),
                                (Clong,toClong y)] 
    in
      ( assign (Cstruct [Clong,Clong]) (offset n (Cstruct [Clong,Clong]) v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a ptarr 0 pts
in
   call3 (getgdi "PolyBezierTo")
         (HDC,POINTER,INT) (INT)
         (h,address ptarr,count)
end  

fun PolyDraw (h,ptlist) = 
let val count = List.length ptlist
    val ptarr = alloc count (Cstruct [Clong,Clong])
    val farr =  alloc count Cchar

    fun pl2a v n [] = () 
    |   pl2a v n ((p,f) :: rest) = 
    let val {x,y} = breakPoint p
        val item = make_struct [(Clong,toClong x),
                                (Clong,toClong y)] 
    in
      ( assign (Cstruct [Clong,Clong]) (offset n (Cstruct [Clong,Clong]) v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a ptarr 0 ptlist

    fun f2a v n [] = () 
    |   f2a v n ((_,f) :: rest) = 
    let
       val item = toCchar (SML90.chr (repE PointTypeE f)) 
    in
      ( assign Cchar (offset n Cchar v) item ;
        f2a v (n+1) rest ) 
    end

    val u = f2a ptarr 0 ptlist
in
   call4 (getgdi "PolyDraw")
         (HDC,POINTER,POINTER,INT) (INT)
         (h,address ptarr,address farr,count)
end

fun Polyline (h,pts) = 
let val count = List.length pts
    val ptarr = alloc count (Cstruct [Clong,Clong])

    fun pl2a v n [] = () 
    |   pl2a v n (p :: rest) = 
    let val {x,y} = breakPoint p
        val item = make_struct [(Clong,toClong x),
                                (Clong,toClong y)] 
    in
      ( assign (Cstruct [Clong,Clong]) (offset n (Cstruct [Clong,Clong]) v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a ptarr 0 pts
in
   call3 (getgdi "Polyline")
         (HDC,POINTER,INT) (BOOL)
         (h,address ptarr,count)
end 


fun PolylineTo (h,pts) = 
let val count = List.length pts
    val ptarr = alloc count (Cstruct [Clong,Clong])

    fun pl2a v n [] = () 
    |   pl2a v n (p :: rest) = 
    let val {x,y} = breakPoint p
        val item = make_struct [(Clong,toClong x),
                                (Clong,toClong y)] 
    in
      ( assign (Cstruct [Clong,Clong]) (offset n (Cstruct [Clong,Clong]) v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a ptarr 0 pts
in
   call3 (getgdi "PolylineTo")
         (HDC,POINTER,INT) (BOOL)
         (h,address ptarr,count)
end

(* PENS *)

fun CreatePen (s,w,Rgb(r,g,b)) =
   call3 (getgdi "CreatePen")
         (PENSTYLE,INT,LONG) (HPEN)
         (s,w,PALETTERGB(r,g,b))
 
fun ExtCreatePen (ps,w,bs,Rgb(r,g,b),hs,intpairl) = 
let val len = List.length intpairl
    val log = make_struct
            [ (Clong,toClong (repE BrushStyleE bs)),
              (Clong,toClong (PALETTERGB(r,g,b))),
              (Clong,toClong (repE HatchStyleE hs))]

    val custom = alloc len Clong

    fun pl2a v n [] = () 
    |   pl2a v n ((l1,l2) :: rest) = 
    let
       val item = toClong (MAKELONG(l1,l2))
    in
      ( assign Clong (offset n Clong v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a custom 0 intpairl

in
   call5 (getgdi "ExtCreatePen")
         (PENSTYLE,LONG,POINTER,LONG,POINTER) (HPEN)
         (ps,w,address log,len,address custom)
end

(* PATHS *)

fun GetPath (h,ptlist) = 
let val count = List.length ptlist
    val ptarr = alloc count (Cstruct [Clong,Clong])
    val farr =  alloc count Cchar

    fun pl2a v n [] = () 
    |   pl2a v n ((p,f) :: rest) = 
    let val {x,y} = breakPoint p
       val item = make_struct [(Clong,toClong x),
                               (Clong,toClong y)] 
    in
      ( assign (Cstruct [Clong,Clong]) (offset n (Cstruct [Clong,Clong]) v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a ptarr 0 ptlist

    fun f2a v n [] = () 
    |   f2a v n ((_,f) :: rest) = 
    let
       val item = toCchar (SML90.chr (repE PointTypeE f)) 
    in
      ( assign Cchar (offset n Cchar v) item ;
        f2a v (n+1) rest ) 
    end

    val u = f2a ptarr 0 ptlist
in
   call4 (getgdi "GetPath")
         (HDC,POINTER,POINTER,INT) (INT)
         (h,address ptarr,address farr,count)
end

(* REGIONS *)

fun CreateEllipticRgn (rect) =
let
  val  {left,top,right,bottom} = breakRect rect
in  
    call4 (getgdi "CreateEllipticRgn")
          (LONG,LONG,LONG,LONG) (HRGN)
          (left,top,right,bottom)
end

fun CreateRectRgn (rect) = 
let
  val  {left,top,right,bottom} = breakRect rect
in
    call4 (getgdi "CreateRectRgn")
          (LONG,LONG,LONG,LONG) (HRGN)
          (left,top,right,bottom)
end

fun CreateRoundRectRgn (rect,w,h) =
let
  val  {left,top,right,bottom} = breakRect rect
in 
    call6 (getgdi "CreateRoundRectRgn")
          (LONG,LONG,LONG,LONG,WIDTH,HEIGHT) (HRGN)
          (left,top,right,bottom,w,h)
end

fun CreatePolygonRgn (pts,fmode) = 
let val count = List.length pts
    val ptarr = alloc count (Cstruct [Clong,Clong])

    fun pl2a v n [] = () 
    |   pl2a v n (p :: rest) = 
    let val {x,y} = breakPoint p
        val item = make_struct [(Clong,toClong x),
                               (Clong,toClong y)] 
    in
      ( assign (Cstruct [Clong,Clong]) (offset n (Cstruct [Clong,Clong]) v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a ptarr 0 pts
in
   call3 (getgdi "CreatePolygonRgn")
         (POINTER,INT,POLYFILLMODE) (HPEN)
         (address ptarr,count,fmode)
end

fun ExtCreateRegion (x,rects,rectmain) =                        
let val {r11,r12,r21,r22,tx,ty} = breakXForm x   
    val xform = make_struct
                  [ (Cfloat,toCfloat r11),
                    (Cfloat,toCfloat r12),
                    (Cfloat,toCfloat r21),
                    (Cfloat,toCfloat r22),
                    (Cfloat,toCfloat tx),
                    (Cfloat,toCfloat ty) ]

    val count = List.length rects

    val rectarr = alloc count (Cstruct [Clong,Clong,Clong,Clong])

    fun pl2a v n [] = () 
    |   pl2a v n (rect :: rest) = 
    let val {left,top,right,bottom} = breakRect rect
        val item = make_struct [(Clong,toClong left),
                                (Clong,toClong top),
                                (Clong,toClong right),
                                (Clong,toClong bottom)] 
    in
      ( assign (Cstruct [Clong,Clong,Clong,Clong]) 
               (offset n (Cstruct [Clong,Clong,Clong,Clong]) v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a rectarr 0 rects
    val {left,top,right,bottom} = breakRect rectmain 

    val rgndata = make_struct
                    [ (Clong,toClong 32),
                      (Clong,toClong 1),
                      (Clong,toClong count),
                      (Clong,toClong 0  ),
                      (Clong,toClong left),
                      (Clong,toClong top),
                      (Clong,toClong right),
                      (Clong,toClong bottom),
                      (Cvoid,rectarr) ]

    val struct_size = 64 + 16 * count
in
  call3 (getgdi "ExtCreateRegion")
         (POINTER,INT,POINTER) (HRGN)
         (address xform,struct_size,address rgndata)
end

fun GetRegionData h =
let
  val bufsize = call3 (getgdi "GetRegionData")
                    (HRGN,LONG,POINTER) (LONG)
                    (h,0,toCint 0)

  val rgndata = alloc 1 (Cstruct [Clong,Clong,Clong,Clong,
                                  Clong,Clong,Clong,Clong,Cvoid])

  val res =  call3 (getgdi "GetRegionData")
                    (HRGN,LONG,POINTER) (LONG)
                    (h,bufsize,address rgndata)
in
  "not implemented"
end 


(* BUTTONS *)

(* COMBO BOXES *)

(* ICONS *)

(* KEYBOARD INPUT *)

fun GetKeyboardState () = 
let val barr = alloc 256 Cchar
    val res =  call1 (getuser "GetKeyboardState")
                     (POINTER) (BOOL)
                     (address barr)

    fun v2l v 0 = []
    |   v2l v i = 
    let val vp = offset (i-1) Cchar v
    in (v2l v (i-1)) @ [SML90.ord (fromCchar vp) ] 
    end                   

in 
   (res,VK (v2l barr 256)) 
end  
            
fun VkKeyScan ch =
let val res = call1 (getuser "VkKeyScanA")
                    (SHORT) (SHORT)
                    (SML90.ord ch)

    val keycode = LOBYTE res
    val vkcode = HIBYTE res

    val r = ref [] : (ShiftState list) ref
    val u = ( if natAnd (1,vkcode) = 1 then r := (Shift :: (!r)) else ();
              if natAnd (2,vkcode) = 2 then r := (Ctrl :: (!r)) else ();
              if natAnd (4,vkcode) = 4 then r := (Alt :: (!r)) else ()   )
in
  (keycode,Comb (!r) )
end 


(* MENUS *)

fun GetMenuCheckMarkDimensions () = 
let
   val res = call0 (getuser "GetMenuCheckMarkDimensions") () (LONG) ()
in
  (LOWORD res,HIWORD res)
end  

fun GetMenuString (h,i,f) = 
let val s = alloc 100 Cchar
    val res = call5 (getuser "GetMenuString")
              (HMENU,INT,POINTER,INT,MENUFLAG) (INT)
              (h,i,address s,100,f)
in
  fromCstring s
end

fun LoadMenuIndirect (mlist) =
let val count = List.length mlist

    val menu = alloc count (Cstruct [Cshort,Cshort,Cpointer Cchar])

    fun pl2a v n [] = () 
    |   pl2a v n (MID {option=flag,
                       id= MenuID id,
                       display=s} :: rest) = 
    let
       val item = make_struct [(Cshort,toCshort (repE MenuFlagE flag)),
                               (Cshort,toCshort id ),
                               (Cpointer Cchar,toCstring s) ] 
    in
      ( assign  (Cstruct [Cshort,Cshort,Cpointer Cchar]) 
      (offset n (Cstruct [Cshort,Cshort,Cpointer Cchar]) v) item ;
        pl2a v (n+1) rest ) 
    end

    val u = pl2a menu 0 mlist

in
  call1 (getuser "LoadMenuIndirectA")
        (POINTER) (HMENU)
        (address menu)
end 
 

(* MOUSE INPUT *)

(* PAINTING AND DRAWING *)

fun BeginPaint h =
let val space = alloc 14 Clong
    val res = call2 (getuser "BeginPaint")
                    (HWND,POINTER) (HDC)
                    (h,address space)
in
   (MLPAINT space,res)
end 

fun EndPaint(h,MLPAINT space) = 
    call2 (getuser "EndPaint")
    (HWND,POINTER) (BOOL)
    (h,address space) 

fun GetBkColor h = 
let val r = call1 (getgdi "GetBkColor")
                  (HDC) (LONG)
                  h
in Rgb(GetRValue r,GetGValue r,GetBValue r)
end

fun SetBkColor (h,Rgb(r,g,b)) = 
let val r = call2 (getgdi "SetBkColor") 
                  (HDC,LONG) (LONG) 
                  (h,PALETTERGB(r,g,b))
in
   Rgb(GetRValue r,GetGValue r,GetBValue r)
end

fun SetRectRgn (h,rect) =
let
  val  {left,top,right,bottom} = breakRect rect
in
   call5 (getgdi "SetRectRgn")
         (HRGN,LONG,LONG,LONG,LONG) (BOOL)
         (h,left,top,right,bottom)
end

(* SCROLL BARS *)

fun GetScrollRange (h,f) = 
let val mv = alloc 1 Cint
    val Mv = alloc 1 Cint
    
    val res = call4 (getuser "GetScrollRange")
              (HWND,SCROLLBARTYPE,POINTER,POINTER)
              (BOOL)
              (h,f,address mv,address Mv) 

in
  Range {min = fromCint mv, 
         max = fromCint Mv} 
end

(* TIMERS *)

val KillTimer = call2 (getuser "KillTimer") (HWND,INT) (BOOL)  

fun SetTimer (h,i,dt) = call4 (getuser "SetTimer") 
                              (HWND,INT,INT,POINTER) (BOOL)
                              (h,i,dt,toCint 0)  
 
(* LIST BOXES *)

(* RECTANGLES *)

(* Extra functions imported from X-Windows interface *)


fun LeftR r = 
let val {left,top,right,bottom} = breakRect r
in left
end

fun RightR r = 
let val {left,top,right,bottom} = breakRect r
in right
end

fun TopR r = 
let val {left,top,right,bottom} = breakRect r
in top
end
fun BottomR r = 
let val {left,top,right,bottom} = breakRect r
in bottom
end
  
fun Width r = 
let val {left,top,right,bottom} = breakRect r
in right - left
end

fun Height r = 
let val {left,top,right,bottom} = breakRect r
in bottom - top
end
  
fun TopRLeftR r = 
let val {left,top,right,bottom} = breakRect r
in  mkPoint{ x=left ,y=top }
end

fun TopRRightR r = 
let val {left,top,right,bottom} = breakRect r
in  mkPoint { x=right,y=top }
end

fun BottomRLeftR r = 
let val {left,top,right,bottom} = breakRect r
in  mkPoint { x=left ,y=bottom }
end

fun BottomRRightR r = 
let val {left,top,right,bottom} = breakRect r
in  mkPoint { x=right,y=bottom }
end
 
fun IsProperRect r =
let val {left,top,right,bottom} = breakRect r
in left <= right andalso top <= bottom
end

fun mkArea {x,y,w,h} = 
  mkRect {left=x,top=y,right=x+w,bottom=y+h}
  
fun breakArea r =
let val {left,top,right,bottom} = breakRect r
in {x=left,y=top,w=right-left,h=bottom-top}
end
  
fun SplitRect r =
let val { top,left,bottom,right } = breakRect r
in ( mkPoint {x=left,y=top},
     mkPoint {x=right,y=bottom})
end

fun min a b = a
fun max a b = a
  
fun MakeRect p1 p2 =
let val {x=x1,y=y1} = breakPoint p1
    val {x=x2,y=y2} = breakPoint p2
in mkRect { left   = min x1 x2,
            top    = min y1 y2,
            right  = max x1 x2,
            bottom = max y1 y2 }
end
  
fun Reflect r =
let val {top,left,bottom,right} = breakRect r
in mkRect { left   = top,
            top    = left,
            right  = bottom,
            bottom = right }
end

fun NegativePoint p = 
let val {x,y} = breakPoint p
in mkPoint {x = ~x, y= ~y}
end

datatype Section = Nothing | Section of Rectangle
 
fun Intersection r1 r2 =
let
   val ML = max (LeftR r1)   (LeftR r2)
   val MT = max (TopR r1)    (TopR r2)
   val MR = min (RightR r1)  (RightR r2)
   val MB = min (BottomR r1) (BottomR r2)
in
   if MR > ML andalso MB > MT then
      Section (mkRect {left=ML,top=MT,right=MR,bottom=MB})
   else
      Nothing
end
    
fun Union r1 r2 =
let
    val ML = min (LeftR r1)   (LeftR r2)
    val MT = min (TopR r1)    (TopR r2)
    val MR = max (RightR r1)  (RightR r2)
    val MB = max (BottomR r1) (BottomR r2)
in
    mkRect { left = ML, top = MT, right = MR, bottom = MB }
end
  
fun OutsetRect dist rect =
let
    val {left,top,right,bottom} = breakRect rect
in
    mkRect { left   = left   - dist,
           top    = top    - dist,
           right  = right  + dist,
           bottom = bottom + dist } 
end
    
fun OffsetRect rect p =
let
  val {x,y} = breakPoint p 
  val { top,left,bottom,right } = breakRect rect
in
    mkRect { left   = left   + x,
             top    = top    + y,
             right  = right  + x,
             bottom = bottom + y } 
end
  
fun IncludePoint p rect =
let
   val {x,y} = breakPoint p   
   val {left,top,right,bottom} = breakRect rect
in
   mkRect { left   = min x left,
            top    = min y top,
            right  = max x right,
            bottom = max y bottom }
end
  
infix AddPoint SubtractPoint

fun p1 AddPoint p2 =
let val{x,y} = breakPoint p1
    val{x=dx,y=dy} = breakPoint p2
in  
    mkPoint { x = x + dx, y = y + dy }
end

fun p1 SubtractPoint p2 =
let val{x,y} = breakPoint p1
    val{x=dx,y=dy} = breakPoint p2
in  
    mkPoint { x = x - dx, y = y - dy }
end  

infix Inside
      Overlap
      Within
      LeftROf
      RightROf
      AboveOf
      BelowOf
      HorizontallyAbutting
      VerticallyAbutting
  
fun r1 Inside r2 =
  (
    LeftR   r1 >= LeftR   r2 andalso
    RightR  r1 <= RightR  r2 andalso
    TopR    r1 >= TopR    r2 andalso
    BottomR r1 <= BottomR r2
  )

fun r1 Overlap r2 = 
 (case (Intersection r1 r2) of 
     Nothing => false 
   | Section _ => true )

fun p Within rect =
let
  val {x,y} = breakPoint p
in
  (
     x >= LeftR   rect andalso
     y >= TopR    rect andalso
     x <  RightR  rect andalso
     y <  BottomR rect
  )
end
  
fun p LeftROf  rect =
let val {x,y} = breakPoint p
in (x <  LeftR   rect)
end

fun p RightROf  rect =
let val {x,y} = breakPoint p
in (x >= RightR   rect)
end

fun p AboveOf  rect =
let val {x,y} = breakPoint p
in (y <  TopR   rect)
end

fun p BelowOf  rect =
let val {x,y} = breakPoint p
in (y >= BottomR   rect)
end


fun a HorizontallyAbutting b = 
  (
    (RightR a = LeftR b orelse LeftR a = RightR b)
    andalso (TopR    a <= TopR b)
    andalso (BottomR a >= BottomR b)
  )
  
fun a VerticallyAbutting b = 
  (
    (BottomR a = TopR b orelse TopR a = BottomR b)
    andalso (LeftR  a <= LeftR b)
    andalso (RightR a >= RightR b)
  )
  
val origin = mkPoint { x=0,y=0 };

val emptyArea = mkArea {x=0,y=0,w=0,h=0} 
val unitArea = mkArea { x=0,y=0,w=1,h=1 };



(* WINDOWS *)

fun LoadIcon id =
let 
   val instance = Getv ("hinst",HANDLE)
in
   call2 (getuser "LoadIconA") (HANDLE,ICONIDENTIFIER) (HICON) 
                              (MLHANDLE (toCint 0),id)
end

fun LoadCursor id = 
let 
   val instance = Getv ("hinst",HANDLE)
in
   call2 (getuser "LoadCursorA") (HANDLE,CURSORIDENTIFIER) (HCURSOR) 
                                (MLHANDLE (toCint 0),id)
end


fun ShowWindow h1 = 
let
   val cmdshow = Getv ("cmdshow",INT)

   fun ShowWin (whandle,cmdshow) = 
      call2 (getuser "ShowWindow")  (HWND,INT) (BOOL) (h1,cmdshow)

in 
   ShowWin (h1,cmdshow)
end;


fun GetWindowPlacement h =
let val p = alloc 1 (Cstruct [Cint,Cint,Cint,
                              Clong,Clong,Clong,Clong,
                              Clong,Clong,Clong,Clong]) 

    val r = call2 (getuser "GetWindowPlacement") 
                  (HWND,POINTER) (BOOL)
                  (h,address p)

    val (le,f,sh,minx,miny,maxx,maxy,l,t,r,b) = 
          break_struct11 [Cint,Cint,Cint,
                        Clong,Clong,Clong,Clong,
                        Clong,Clong,Clong,Clong] p

in
  (absE WindowPlacementFlagE (fromCint f),
   absE ShowWindowModeE (fromCint sh),
   mkPoint {x=fromClong minx,y=fromClong miny},
   mkPoint {x=fromClong maxx,y=fromClong maxy},
   mkRect {left=fromClong l,top=fromClong t,
           right=fromClong r,bottom=fromClong b})   
end 



fun SetWindowPlacement (h,f,sh,p1,p2,rect) = 
let val {left,top,right,bottom} = breakRect rect
    val {x=x1,y=y1} = breakPoint p1
    val {x=x2,y=y2} = breakPoint p2

    val p = make_struct ([(Cint,toCint 48 ),
                          (Cint,toCint (repE WindowPlacementFlagE f) ),
                          (Cint,toCint (repE ShowWindowModeE sh ) ),
                          (Clong,toClong x1),
                          (Clong,toClong y1),
                          (Clong,toClong x2),
                          (Clong,toClong y2),
                          (Clong,toClong left),
                          (Clong,toClong top),
                          (Clong,toClong right),
                          (Clong,toClong bottom) ]) 
in
    call2 (getuser "SetWindowPlacement") 
          (HWND,POINTER) (BOOL)
          (h,address p)
end


end
end;


