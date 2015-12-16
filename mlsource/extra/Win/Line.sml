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
    val LineTo : HDC * POINT -> unit
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
        open Foreign Base GdiBase
        
        val zeroPoint: POINT = {x=0, y=0}
    in
        type HDC = HDC and POINT = POINT and RECT = RECT

        datatype PointType = datatype PointType

        local
            datatype ArcDirection =
            W of int
        in
            type ArcDirection = ArcDirection
            val ARCDIRECTION = absConversion {abs = W, rep = fn W n => n} cInt
        
            val AD_COUNTERCLOCKWISE                          = W(1)
            val AD_CLOCKWISE                                 = W(2)
        end;

        val AngleArc         = winCall6(gdi "AngleArc") (cHDC,cInt,cInt,cDWORD,cFloat,cFloat) (successState "AngleArc")
        val Arc              = winCall9(gdi "Arc") (cHDC,cInt,cInt,cInt,cInt,cInt,cInt,cInt,cInt) (successState "Arc")
        val ArcTo            = winCall9(gdi "ArcTo") (cHDC,cInt,cInt,cInt,cInt,cInt,cInt,cInt,cInt) (successState "ArcTo")
        val GetArcDirection  = winCall1(gdi "GetArcDirection") (cHDC) ARCDIRECTION
        val SetArcDirection  = winCall2(gdi "SetArcDirection") (cHDC,ARCDIRECTION) ARCDIRECTION

        local
            val lineTo = winCall3 (gdi "LineTo") (cHDC,cInt,cInt) (successState "LineTo")
        in
            fun LineTo (h,({x,y}:POINT)) = lineTo (h,x,y)
        end

        local
            val moveToEx = winCall4 (gdi "MoveToEx") (cHDC, cInt, cInt, cStar cPoint) (successState "MoveToEx")
        in
            fun MoveToEx(h, ({x,y}:POINT)) =
                let val p = ref zeroPoint in moveToEx(h, x, y, p); !p end
        end

        local
            val polyBezier = winCall3 (gdi "PolyBezier") (cHDC,cPointer,cDWORD) (successState "PolyBezier")
            and polyBezierTo = winCall3 (gdi "PolyBezierTo") (cHDC,cPointer,cDWORD) (successState "PolyBezierTo")
            and polyDraw = winCall4 (gdi "PolyDraw") (cHDC,cPointer,cPointer, cInt) (successState "PolyDraw")
            and polyLine = winCall3 (gdi "Polyline") (cHDC,cPointer,cInt) (successState "Polyline")
            and polyLineTo = winCall3 (gdi "PolylineTo") (cHDC,cPointer,cDWORD) (successState "PolylineTo")

            val ptList = list2Vector cPoint
            val pTypeList = list2Vector cPOINTTYPE
        in
            fun PolyBezier (h, pts) = 
            let
                val (ptarr, count) = ptList pts
            in
                polyBezier(h, ptarr, count) handle ex => (Memory.free ptarr; raise ex);
                Memory.free ptarr
            end

            and PolyBezierTo (h, pts) = 
            let
                val (ptarr, count) = ptList pts
            in
                polyBezierTo(h, ptarr, count) handle ex => (Memory.free ptarr; raise ex);
                Memory.free ptarr
            end
            
            and PolyDraw (h, tplist: (PointType * POINT) list) = 
            let
                val (typeList, pl) = ListPair.unzip tplist
                val (ptarr, count) = ptList pl
                val (farr, _) = pTypeList typeList
            in
                polyDraw(h, ptarr, farr,count) handle ex => (Memory.free ptarr; Memory.free farr; raise ex);
                Memory.free ptarr; Memory.free farr
            end
            
            and Polyline (h, pts: POINT list) =
            let
                val (ptarr, count) = ptList pts
            in
                polyLine(h, ptarr, count) handle ex => (Memory.free ptarr; raise ex);
                Memory.free ptarr
            end

            and PolylineTo (h, pts: POINT list) =
            let
                val (ptarr, count) = ptList pts
            in
                polyLineTo(h, ptarr, count) handle ex => (Memory.free ptarr; raise ex);
                Memory.free ptarr
            end
        end

        (*
        Other Line and Curve functions:
            LineDDA  
            LineDDAProc  
            PolyPolyline
        *)

    end
end;
