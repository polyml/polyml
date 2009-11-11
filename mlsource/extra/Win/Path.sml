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
        open CInterface Base

        fun callgdi name = call_sym (load_sym (load_lib "gdi32.DLL") name)

        fun gdicall_IW name CR (C1,C2) (a1) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = address (alloc 1 ctype2)
                val res = callgdi name [(ctype1,va1),(Cpointer ctype2,va2)] ctypeR
                val _: unit = fromR res
            in  (from2 (deref va2))
            end
        fun gdicall_IIW name CR (C1,C2,C3) (a1,a2) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = address (alloc 1 ctype3)
                val res = callgdi name [(ctype1,va1),(ctype2,va2),(Cpointer ctype3,va3)] ctypeR
                val _ : unit = fromR res
            in  (from3 (deref va3))
            end
    in
        type HDC = HDC and POINT = POINT and HRGN = HRGN
        datatype PointType = datatype Line.PointType

        (* PATHS *)
        val AbortPath                  = call1(gdi "AbortPath") (HDC) (SUCCESSSTATE "AbortPath")
        val BeginPath                  = call1(gdi "BeginPath") (HDC) (SUCCESSSTATE "BeginPath")
        val CloseFigure                = call1(gdi "CloseFigure") (HDC) (SUCCESSSTATE "CloseFigure")
        val EndPath                    = call1(gdi "EndPath") (HDC) (SUCCESSSTATE "EndPath")
        val FillPath                   = call1(gdi "FillPath") (HDC) (SUCCESSSTATE "FillPath")
        val FlattenPath                = call1(gdi "FlattenPath") (HDC) (SUCCESSSTATE "FlattenPath")
        val GetMiterLimit              = gdicall_IW "GetMiterLimit" (SUCCESSSTATE "GetMiterLimit") (HDC,FLOAT)
        val PathToRegion               = call1(gdi "PathToRegion") (HDC) HRGN
        val SetMiterLimit              = gdicall_IIW "SetMiterLimit" (SUCCESSSTATE "SetMiterLimit") (HDC,FLOAT,FLOAT)
        val StrokeAndFillPath          = call1(gdi "StrokeAndFillPath") (HDC) (SUCCESSSTATE "StrokeAndFillPath")
        val StrokePath                 = call1(gdi "StrokePath") (HDC) (SUCCESSSTATE "StrokePath")
        val WidenPath                  = call1(gdi "WidenPath") (HDC) (SUCCESSSTATE "WidenPath")

        fun GetPath h =
        let
            val gpCall = call4 (gdi "GetPath") (HDC, POINTER, POINTER, INT) INT
            val null = toCint 0
            (* Passing 0 as the size will retrieve the number of points. *)
            val count = gpCall(h, null, null, 0)
            val _ = checkResult(count >= 0)

            val (fromPt, _, ptStruct) = breakConversion POINT
            val (fromTy, _, _) = breakConversion GdiBase.POINTTYPE
            val ptarr = alloc count ptStruct
            val farr =  alloc count Cchar
            val res = gpCall(h, address ptarr, address farr, count)
            fun getElement(n:int): PointType * POINT =
                (fromTy(deref(offset n Cchar farr)), fromPt(deref(offset n ptStruct ptarr)))
        in
            List.tabulate(count, getElement) 
        end

    end
end;
