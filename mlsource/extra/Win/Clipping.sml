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

structure Clipping :
  sig
    type HDC and HRGN
    type RECT = { top: int, left: int, bottom: int, right: int }
    type RegionOperation = Region.RegionOperation
    type ResultRegion = Region.ResultRegion

    val ExcludeClipRect : HDC * RECT -> ResultRegion
    val ExtSelectClipRgn : HDC * HRGN * RegionOperation -> ResultRegion
    val GetClipBox : HDC -> ResultRegion * RECT
    val GetClipRgn : HDC * HRGN -> unit
    val GetMetaRgn : HDC * HRGN -> unit
    val IntersectClipRect : HDC * RECT -> ResultRegion
    val OffsetClipRgn : HDC * int * int -> ResultRegion
    val PtVisible : HDC * int * int * bool -> bool
    val RectVisible : HDC * RECT -> bool
    val SelectClipPath : HDC * RegionOperation -> unit
    val SelectClipRgn : HDC * HRGN -> unit
    val SetMetaRgn : HDC -> unit
  end =
struct
    local
        open CInterface Base GdiBase
        val XCOORD = INT : int Conversion
        val YCOORD = INT: int Conversion
        val XOFFSET = INT: int Conversion
        val YOFFSET = INT: int Conversion
    in
        type RegionOperation = RegionOperation and ResultRegion = ResultRegion
        type RECT = RECT and HDC = HDC and HRGN = HRGN
        
        val ExtSelectClipRgn           = call3(gdi "ExtSelectClipRgn") (HDC,HRGN,REGIONOPERATION) RESULTREGION
        val GetClipRgn                 = call2(gdi "GetClipRgn") (HDC,HRGN) (SUCCESSSTATE "GetClipRgn")
        val GetMetaRgn                 = call2(gdi "GetMetaRgn") (HDC,HRGN) (SUCCESSSTATE "GetMetaRgn")
        val OffsetClipRgn              = call3(gdi "OffsetClipRgn") (HDC,XOFFSET,YOFFSET) RESULTREGION
        val RectVisible                = call2(gdi "RectVisible") (HDC,POINTERTO RECT) BOOL
        val PtVisible                  = call4(gdi "PtVisible") (HDC,XCOORD,YCOORD,BOOL) BOOL
        val SelectClipPath             = call2(gdi "SelectClipPath") (HDC,REGIONOPERATION) (SUCCESSSTATE "SelectClipPath")
        val SelectClipRgn              = call2(gdi "SelectClipRgn") (HDC,HRGN) (SUCCESSSTATE "SelectClipRgn")
        val SetMetaRgn                 = call1(gdi "SetMetaRgn") (HDC) (SUCCESSSTATE "SetMetaRgn")

        fun ExcludeClipRect (h,{left,top,right,bottom}: RECT) =
            call5 (gdi "ExcludeClipRect") (HDC,LONG,LONG,LONG,LONG) RESULTREGION
                       (h,left,top,right,bottom)
         
        fun IntersectClipRect (h,{left,top,right,bottom}: RECT) =
              call5 (gdi "IntersectClipRect")
                       (HDC,LONG,LONG,LONG,LONG) RESULTREGION (h,left,top,right,bottom)

        fun GetClipBox hdc =
        let
            val (from,_,rtype) = breakConversion RECT 
            val va = address (alloc 1 rtype)
            val res = call2 (gdi "GetClipBox") (HDC, POINTER) RESULTREGION (hdc, va)
        in
            (res, from(deref va))
        end

    end
end;
