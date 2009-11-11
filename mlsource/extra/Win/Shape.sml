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
        open CInterface Base
        val WIDTH = INT: int Conversion
        val HEIGHT = INT: int Conversion
    in
        type HDC = HDC and HBRUSH = HBRUSH
        type RECT = RECT and POINT = POINT
        (* FILLED SHAPES *)
        (* Strangely, some of these are in user32 and some in gdi32. *)
        val FillRect             = call3 (user "FillRect") (HDC,POINTERTO RECT,HBRUSH) (SUCCESSSTATE "FillRect")
        val FrameRect            = call3 (user "FrameRect") (HDC,POINTERTO RECT,HBRUSH) (SUCCESSSTATE "FrameRect")
        val InvertRect           = call2 (user "InvertRect")  (HDC,POINTERTO RECT) (SUCCESSSTATE "InvertRect")
        
        fun Chord (h,{left,top,right,bottom}: RECT,{x=x1,y=y1}: POINT,{x=x2,y=y2}: POINT) =
           call9 (gdi "Chord")
                 (HDC,LONG,LONG,LONG,LONG,LONG,LONG,LONG,LONG) (SUCCESSSTATE "Chord")
                 (h,left,top,right,bottom,x1,y1,x2,y2)
        
        fun Ellipse (h,{left,top,right,bottom}: RECT) =
            call5 (gdi "Ellipse")
                 (HDC,LONG,LONG,LONG,LONG) (SUCCESSSTATE "Ellipse")
                 (h,left,top,right,bottom)
        
        fun Pie (h,{left,top,right,bottom}: RECT,{x=x1,y=y1}: POINT,{x=x2,y=y2}: POINT) =
           call9 (gdi "Pie")
                 (HDC,LONG,LONG,LONG,LONG,LONG,LONG,LONG,LONG) (SUCCESSSTATE "Pie")
                 (h,left,top,right,bottom,x1,y1,x2,y2)
        
        fun Polygon (h,pts: POINT list) = 
        let val count = List.length pts
            val (fromPt, toPt, ptStruct) = breakConversion POINT
            val ptarr = alloc count ptStruct
            fun setItem(pt, n) =
                (assign ptStruct (offset n ptStruct ptarr) (toPt pt); n+1)
            val _: int = List.foldl setItem 0 pts
        in
           call3 (gdi "Polygon") (HDC,POINTER,INT) (SUCCESSSTATE "Polygon") (h,address ptarr,count)
        end
        
        fun Rectangle(h,{left,top,right,bottom}: RECT) =
            call5 (gdi "Rectangle")
                  (HDC,LONG,LONG,LONG,LONG) (SUCCESSSTATE "Rectangle")
                  (h,left,top,right,bottom)
        
        fun RoundRect(h,{left,top,right,bottom}: RECT,w,ht) =
            call7 (gdi "RoundRect")
                  (HDC,LONG,LONG,LONG,LONG,WIDTH,HEIGHT) (SUCCESSSTATE "RoundRect")
                  (h,left,top,right,bottom,w,ht)
 
        (*
        Other Filled shape functions:
            PolyPolygon  
        *)
        
    end
end;
