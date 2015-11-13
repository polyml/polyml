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

structure Shape:
  sig
    type HBRUSH
    type HDC
    type POINT = { x: int, y: int }
    type RECT = { top: int, left: int, bottom: int, right: int }

    val Chord : HDC * RECT * POINT * POINT -> unit
    val Ellipse : HDC * RECT -> unit
    val FillRect : HDC * RECT * HBRUSH -> unit
    val FrameRect : HDC * RECT * HBRUSH -> unit
    val InvertRect : HDC * RECT -> unit
    val Pie : HDC * RECT * POINT * POINT -> unit
    val Polygon : HDC * POINT list -> unit
    val Rectangle : HDC * RECT -> unit
    val RoundRect : HDC * RECT * int * int -> unit
  end =
struct
    local
        open Foreign Base
    in
        type HDC = HDC and HBRUSH = HBRUSH
        type RECT = RECT and POINT = POINT
        (* FILLED SHAPES *)
        (* Strangely, some of these are in user32 and some in gdi32. *)
        val FillRect             = call3 (user "FillRect") (cHDC,cConstStar cRect,cHBRUSH) (successState "FillRect")
        val FrameRect            = call3 (user "FrameRect") (cHDC,cConstStar cRect,cHBRUSH) (successState "FrameRect")
        val InvertRect           = call2 (user "InvertRect")  (cHDC,cConstStar cRect) (successState "InvertRect")
        
        local
            val chord =
                call9 (gdi "Chord") (cHDC,cInt,cInt,cInt,cInt,cInt,cInt,cInt,cInt) (successState "Chord")
        in
            fun Chord (h,{left,top,right,bottom}: RECT,{x=x1,y=y1}: POINT,{x=x2,y=y2}: POINT) =
                chord (h,left,top,right,bottom,x1,y1,x2,y2)
        end
        
        local
            val ellipse =
                call5 (gdi "Ellipse") (cHDC,cInt,cInt,cInt,cInt) (successState "Ellipse")
        in
            fun Ellipse (h,{left,top,right,bottom}: RECT) =
                ellipse(h,left,top,right,bottom)
        end
        
        local
            val pie =
                call9 (gdi "Pie")
                    (cHDC,cInt,cInt,cInt,cInt,cInt,cInt,cInt,cInt) (successState "Pie")
        in
            fun Pie (h,{left,top,right,bottom}: RECT,{x=x1,y=y1}: POINT,{x=x2,y=y2}: POINT) =
                pie(h,left,top,right,bottom,x1,y1,x2,y2)
        end
        
        local
            val polygon = call3 (gdi "Polygon") (cHDC,cPointer,cInt) (successState "Polygon")
            val ptList = list2Vector cPoint
        in
            fun Polygon (h,pts: POINT list) = 
            let
                val (ptarr, count) = ptList pts
            in
                polygon (h, ptarr, count) handle ex => (Memory.free ptarr; raise ex);
                Memory.free ptarr
            end
        end
        
        local
            val rectangle =
                call5 (gdi "Rectangle") (cHDC,cInt,cInt,cInt,cInt) (successState "Rectangle")
        in
            fun Rectangle(h,{left,top,right,bottom}: RECT) =
                rectangle(h,left,top,right,bottom)
        end
        
        local
            val roundRect =
                call7 (gdi "RoundRect") (cHDC,cInt,cInt,cInt,cInt,cInt,cInt) (successState "RoundRect")
        in
            fun RoundRect(h,{left,top,right,bottom}: RECT,w,ht) =
                roundRect(h,left,top,right,bottom,w,ht)
        end
 
        (*
        Other Filled shape functions:
            PolyPolygon  
        *)
        
    end
end;
