(*
    Copyright (c) 2001, 2015
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

structure Clipping :
  sig
    type HDC and HRGN
    type RECT = { top: int, left: int, bottom: int, right: int }
    type POINT = { x: int, y: int }
    type RegionOperation = Region.RegionOperation
    type ResultRegion = Region.ResultRegion

    val ExcludeClipRect : HDC * RECT -> ResultRegion
    val ExtSelectClipRgn : HDC * HRGN * RegionOperation -> ResultRegion
    val GetClipBox : HDC -> ResultRegion * RECT
    val GetClipRgn : HDC * HRGN -> unit
    val GetMetaRgn : HDC * HRGN -> unit
    val IntersectClipRect : HDC * RECT -> ResultRegion
    val OffsetClipRgn : HDC * int * int -> ResultRegion
    val PtVisible : HDC * POINT -> bool
    val RectVisible : HDC * RECT -> bool
    val SelectClipPath : HDC * RegionOperation -> unit
    val SelectClipRgn : HDC * HRGN -> unit
    val SetMetaRgn : HDC -> unit
  end =
struct
    local
        open Foreign Base GdiBase
    in
        type RegionOperation = RegionOperation and ResultRegion = ResultRegion
        type RECT = RECT and HDC = HDC and HRGN = HRGN and POINT = POINT
        
        val ExtSelectClipRgn           = call3(gdi "ExtSelectClipRgn") (cHDC,cHRGN,REGIONOPERATION) RESULTREGION
        val GetClipRgn                 = call2(gdi "GetClipRgn") (cHDC,cHRGN) (successState "GetClipRgn")
        val GetMetaRgn                 = call2(gdi "GetMetaRgn") (cHDC,cHRGN) (successState "GetMetaRgn")
        val OffsetClipRgn              = call3(gdi "OffsetClipRgn") (cHDC,cInt,cInt) RESULTREGION
        val RectVisible                = call2(gdi "RectVisible") (cHDC,cConstStar cRect) cBool
        val SelectClipPath             = call2(gdi "SelectClipPath") (cHDC,REGIONOPERATION) (successState "SelectClipPath")
        val SelectClipRgn              = call2(gdi "SelectClipRgn") (cHDC,cHRGN) (successState "SelectClipRgn")
        val SetMetaRgn                 = call1(gdi "SetMetaRgn") (cHDC) (successState "SetMetaRgn")
        
        local
            val ptVisible = call3(gdi "PtVisible") (cHDC,cInt,cInt) cBool
        in
            fun PtVisible(hd, {x, y}) = ptVisible(hd, x, y)
        end

        local
            val excludeClipRect = call5 (gdi "ExcludeClipRect") (cHDC,cInt,cInt,cInt,cInt) RESULTREGION
        in
            fun ExcludeClipRect (h,{left,top,right,bottom}) = excludeClipRect(h,left,top,right,bottom)
        end

        local
            val intersectClipRect =
                call5 (gdi "IntersectClipRect") (cHDC,cInt,cInt,cInt,cInt) RESULTREGION
        in
            fun IntersectClipRect (h,{left,top,right,bottom}: RECT) =
               intersectClipRect(h,left,top,right,bottom)
        end

        local
            val getClipBox = call2 (gdi "GetClipBox") (cHDC, cStar cRect) RESULTREGION
            val zeroRect = { top=0, bottom=0, left=0, right=0}
        in
            fun GetClipBox hdc =
                let val v = ref zeroRect val res = getClipBox(hdc, v) in (res, !v) end
        end

    end
end;
