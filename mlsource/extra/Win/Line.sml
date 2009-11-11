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

structure Line :
  sig
    type HDC
    type RECT = { top: int, left: int, bottom: int, right: int }
    type POINT = { x: int, y: int }

    datatype PointType =
          PT_BEZIERTO
        | PT_BEZIERTOANDCLOSE
        | PT_LINETO
        | PT_LINETOANDCLOSE
        | PT_MOVETO

    eqtype ArcDirection
    val AD_CLOCKWISE : ArcDirection
    val AD_COUNTERCLOCKWISE : ArcDirection

    val AngleArc : HDC * int * int * int * real * real -> unit
    val Arc : HDC * int * int * int * int * int * int * int * int -> unit
    val ArcTo : HDC * int * int * int * int * int * int * int * int -> unit
    val GetArcDirection : HDC -> ArcDirection
    val LineTo : HDC * POINT -> bool
    val MoveToEx : HDC * POINT -> POINT
    val PolyBezier : HDC * POINT list -> unit
    val PolyBezierTo : HDC * POINT list -> unit
    val PolyDraw : HDC * (PointType * POINT) list -> unit
    val Polyline : HDC * POINT list -> unit
    val PolylineTo : HDC * POINT list -> unit
    val SetArcDirection : HDC * ArcDirection -> ArcDirection

  end =
struct
    local
        open CInterface Base GdiBase

        val (fromPt, toPt, ptStruct) = breakConversion POINT

        val XCOORD = INT : int Conversion
        val YCOORD = INT: int Conversion
    in
        type HDC = HDC and POINT = POINT and RECT = RECT

        datatype PointType = datatype PointType

        local
            datatype ArcDirection =
            W of int
        in
            type ArcDirection = ArcDirection
            val ARCDIRECTION = absConversion {abs = W, rep = fn W n => n} INT
        
            val AD_COUNTERCLOCKWISE                          = W(1)
            val AD_CLOCKWISE                                 = W(2)
        end;

        val AngleArc         = call6(gdi "AngleArc") (HDC,XCOORD,YCOORD,INT,FLOAT,FLOAT) (SUCCESSSTATE "AngleArc")
        val Arc              = call9(gdi "Arc") (HDC,INT,INT,INT,INT,INT,INT,INT,INT) (SUCCESSSTATE "Arc")
        val ArcTo            = call9(gdi "ArcTo") (HDC,INT,INT,INT,INT,INT,INT,INT,INT) (SUCCESSSTATE "ArcTo")
        val GetArcDirection  = call1(gdi "GetArcDirection") (HDC) ARCDIRECTION
        val SetArcDirection  = call2(gdi "SetArcDirection") (HDC,ARCDIRECTION) ARCDIRECTION
        fun LineTo (h,({x,y}:POINT)) = call3 (gdi "LineTo") (HDC,INT,INT) (BOOL) (h,x,y)

        local
            val moveToEx = call4 (gdi "MoveToEx") (HDC, INT, INT, POINTER) (SUCCESSSTATE "MoveToEx")
        in
            fun MoveToEx(h, ({x,y}:POINT)) =
            let
                val p = alloc 1 ptStruct
                val _ = moveToEx(h, x, y, address p)
            in
                fromPt p
            end
        end

        fun PolyBezier (h,pts: POINT list) = 
        let val count = List.length pts
            val ptarr = alloc count ptStruct
            fun setItem(pt, n) =
                (assign ptStruct (offset n ptStruct ptarr) (toPt pt); n+1)
            val _: int = List.foldl setItem 0 pts
        in
           call3 (gdi "PolyBezier") (HDC,POINTER,INT) (SUCCESSSTATE "PolyBezier") (h,address ptarr,count)
        end 
        
        fun PolyBezierTo (h,pts: POINT list) = 
        let val count = List.length pts
            val ptarr = alloc count ptStruct
            fun setItem(pt, n) =
                (assign ptStruct (offset n ptStruct ptarr) (toPt pt); n+1)
            val _: int = List.foldl setItem 0 pts
        in
           call3 (gdi "PolyBezierTo") (HDC,POINTER,INT) (SUCCESSSTATE "PolyBezierTo") (h,address ptarr,count)
        end 

        fun PolyDraw (h, tplist: (PointType * POINT) list) = 
        let
            val (typeList, ptList) = ListPair.unzip tplist
            val (ptarr, count) = list2Vector POINT ptList
            val (farr, _) = list2Vector POINTTYPE typeList
        in
           call4 (gdi "PolyDraw")
                 (HDC,POINTER,POINTER,INT) (SUCCESSSTATE "PolyDraw")
                 (h, ptarr, farr,count)
        end
        
        fun Polyline (h, pts: POINT list) = 
        let
            val (ptarr, count) = list2Vector POINT pts
        in
            call3 (gdi "Polyline") (HDC,POINTER,INT) (SUCCESSSTATE "Polyline") (h, ptarr, count)
        end
        
        fun PolylineTo (h,pts: POINT list) = 
        let
            val (ptarr, count) = list2Vector POINT pts
        in
            call3 (gdi "PolylineTo") (HDC,POINTER,INT) (SUCCESSSTATE "PolylineTo") (h, ptarr, count)
        end
        (*
        Other Line and Curve functions:
            LineDDA  
            LineDDAProc  
            PolyPolyline
        *)

    end
end;
