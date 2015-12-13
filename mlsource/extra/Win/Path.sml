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

structure Path :
  sig
    type HDC and HRGN
    type POINT = {x: int, y: int}
    datatype PointType = datatype Line.PointType

    val AbortPath : HDC -> unit
    val BeginPath : HDC -> unit
    val CloseFigure : HDC -> unit
    val EndPath : HDC -> unit
    val FillPath : HDC -> unit
    val FlattenPath : HDC -> unit
    val GetMiterLimit : HDC -> real
    val GetPath : HDC -> (PointType * POINT) list
    val PathToRegion : HDC -> HRGN
    val SetMiterLimit : HDC * real -> real
    val StrokeAndFillPath : HDC -> unit
    val StrokePath : HDC -> unit
    val WidenPath : HDC -> unit

  end =
struct
    local
        open Foreign Base
    in
        type HDC = HDC and POINT = POINT and HRGN = HRGN
        datatype PointType = datatype Line.PointType

        (* PATHS *)
        val AbortPath                  = winCall1(gdi "AbortPath") (cHDC) (successState "AbortPath")
        val BeginPath                  = winCall1(gdi "BeginPath") (cHDC) (successState "BeginPath")
        val CloseFigure                = winCall1(gdi "CloseFigure") (cHDC) (successState "CloseFigure")
        val EndPath                    = winCall1(gdi "EndPath") (cHDC) (successState "EndPath")
        val FillPath                   = winCall1(gdi "FillPath") (cHDC) (successState "FillPath")
        val FlattenPath                = winCall1(gdi "FlattenPath") (cHDC) (successState "FlattenPath")
        val PathToRegion               = winCall1(gdi "PathToRegion") (cHDC) cHRGN
        val StrokeAndFillPath          = winCall1(gdi "StrokeAndFillPath") (cHDC) (successState "StrokeAndFillPath")
        val StrokePath                 = winCall1(gdi "StrokePath") (cHDC) (successState "StrokePath")
        val WidenPath                  = winCall1(gdi "WidenPath") (cHDC) (successState "WidenPath")
        
        local
            val getMiterLimit = winCall2(gdi "GetMiterLimit") (cHDC, cStar cFloat) (successState "GetMiterLimit")
            and setMiterLimit = winCall3(gdi "SetMiterLimit") (cHDC, cFloat, cStar cFloat) (successState "SetMiterLimit")
        in
            fun GetMiterLimit hdc = let val v = ref 0.0 in getMiterLimit(hdc, v); !v end
            and SetMiterLimit(hdc, m) = let val v = ref 0.0 in setMiterLimit(hdc, m, v); !v end
        end

        local
            val getPath = winCall4 (gdi "GetPath") (cHDC, cPointer, cPointer, cInt) cInt
            val {load=fromPt, ctype={size=sizePt, ...}, ...} = breakConversion cPoint
            val {load=fromTy, ...} = breakConversion GdiBase.cPOINTTYPE
        in
            fun GetPath h =
            let
                open Memory
                infix 6 ++
                (* Passing 0 as the size will retrieve the number of points. *)
                val count = getPath(h, null, null, 0)
                val _ = checkResult(count >= 0)

                val ptarr = malloc(Word.fromInt count * sizePt)
                val farr =  malloc(Word.fromInt count)
                val _ = getPath(h, ptarr, farr, count) handle ex => (free ptarr; free farr; raise ex)
                fun getElement n =
                    (fromTy(farr ++ Word.fromInt n), fromPt(ptarr ++ Word.fromInt n * sizePt))
            in
                List.tabulate(count, getElement) before (free ptarr; free farr)
            end
        end

    end
end;
