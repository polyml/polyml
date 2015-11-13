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

structure Region:
sig
    type HDC and HBRUSH and HRGN and HPEN
    type POINT = { x: int, y: int }
    type RECT = { top: int, left: int, bottom: int, right: int }

    type RegionOperation
    val RGN_AND : RegionOperation
    val RGN_COPY : RegionOperation
    val RGN_DIFF : RegionOperation
    val RGN_ERROR : RegionOperation
    val RGN_OR : RegionOperation
    val RGN_XOR : RegionOperation

    type ResultRegion
    val COMPLEXREGION : ResultRegion
    val NULLREGION : ResultRegion
    val ERROR : ResultRegion
    val SIMPLEREGION : ResultRegion

    type PolyFillMode
    val ALTERNATE : PolyFillMode
    val WINDING : PolyFillMode

    val CombineRgn : HRGN * HRGN * HRGN * RegionOperation -> ResultRegion
    val CreateEllipticRgn : RECT -> HRGN
    val CreatePolygonRgn : POINT list * PolyFillMode -> HRGN
    val CreateRectRgn : RECT -> HRGN
    val CreateRoundRectRgn : RECT * int * int -> HRGN
    val EqualRgn : HRGN * HRGN -> bool
    val FillRgn : HDC * HRGN * HBRUSH -> unit
    val FrameRgn : HDC * HRGN * HBRUSH * int * int -> unit
    val GetPolyFillMode : HDC -> PolyFillMode
    val GetRgnBox : HRGN -> RECT
    val InvertRgn : HDC * HRGN -> unit
    val OffsetRgn : HRGN * int * int -> ResultRegion
    val PaintRgn : HDC * HRGN -> unit
    val PtInRegion : HRGN * int * int -> bool
    val RectInRegion : HRGN * RECT -> bool
    val SetPolyFillMode : HDC * PolyFillMode -> PolyFillMode
    val SetRectRgn : HRGN * RECT -> unit

  end =
struct
    local
        open Foreign Base
    in
        type HRGN = Base.HRGN and HBRUSH = Base.HBRUSH and HDC = Base.HDC
        and HPEN = HPEN and RECT = RECT and POINT = POINT

        open GdiBase

        local
            datatype PolyFillMode =
            W of int
        in
            type PolyFillMode = PolyFillMode
            val POLYFILLMODE = absConversion {abs = W, rep = fn W n => n} cInt
        
            val ALTERNATE                                    = W (1)
            val WINDING                                      = W (2)
        end

        val CombineRgn                 = call4(gdi "CombineRgn") (cHRGN,cHRGN,cHRGN,REGIONOPERATION) RESULTREGION 
        val EqualRgn                   = call2(gdi "EqualRgn") (cHRGN,cHRGN) cBool
        val FillRgn                    = call3(gdi "FillRgn") (cHDC,cHRGN,cHBRUSH) (successState "FillRgn")
        val FrameRgn                   = call5(gdi "FrameRgn") (cHDC,cHRGN,cHBRUSH,cInt,cInt) (successState "FrameRgn")
        val GetPolyFillMode            = call1(gdi "GetPolyFillMode") (cHDC) POLYFILLMODE
        val InvertRgn                  = call2(gdi "InvertRgn") (cHDC,cHRGN) (successState "InvertRgn")
        val OffsetRgn                  = call3(gdi "OffsetRgn") (cHRGN,cInt,cInt) RESULTREGION
        val PaintRgn                   = call2(gdi "PaintRgn") (cHDC,cHRGN) (successState "PaintRgn")
        val PtInRegion                 = call3(gdi "PtInRegion") (cHRGN,cInt,cInt) cBool
        val RectInRegion               = call2(gdi "RectInRegion") (cHRGN,cRect) cBool
        val SetPolyFillMode            = call2(gdi "SetPolyFillMode") (cHDC,POLYFILLMODE) POLYFILLMODE

        local
            val getRgnBox = call2(gdi "GetRgnBox") (cHRGN, cStar cRect) cInt
            val zeroRect = {top=0, bottom=0, left=0, right=0}
        in
            fun GetRgnBox hr =
            let val v = ref zeroRect in checkResult(getRgnBox(hr, v) <> 0); !v end
        end

        local
            val setRectRgn = call5 (gdi "SetRectRgn") (cHRGN,cInt,cInt,cInt,cInt)  (successState "SetRectRgn")
        in
            fun SetRectRgn (h, { left, top, right, bottom }) = setRectRgn(h,left,top,right,bottom)
        end
        
        local
            val createEllipticRgn = call4 (gdi "CreateEllipticRgn") (cInt,cInt,cInt,cInt) cHRGN
        in
            fun CreateEllipticRgn {left,top,right,bottom} = createEllipticRgn(left,top,right,bottom)
        end
        
        local
            val createRectRgn = call4 (gdi "CreateRectRgn") (cInt,cInt,cInt,cInt) cHRGN
        in
            fun CreateRectRgn {left,top,right,bottom} = createRectRgn(left,top,right,bottom)
        end
        
        local
            val createRoundRectRgn = call6 (gdi "CreateRoundRectRgn") (cInt,cInt,cInt,cInt,cInt,cInt) cHRGN
        in
            fun CreateRoundRectRgn({left,top,right,bottom},w,h) =
                createRoundRectRgn(left,top,right,bottom,w,h)
        end

        local
            val createPolygonRgn = call3 (gdi "CreatePolygonRgn") (cPointer,cInt,POLYFILLMODE) cHRGN
            val ptList = list2Vector cPoint
        in
            fun CreatePolygonRgn (pts: POINT list, fmode) = 
            let
                val (ptarr, count) = ptList pts
            in
                (createPolygonRgn(ptarr,count,fmode) handle ex => (Memory.free ptarr; raise ex))
                    before Memory.free ptarr
            end
        end
    
(*      fun ExtCreateRegion (x,rects,rectmain) =                        
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
            |   pl2a v n ({left,top,right,bottom} :: rest) = 
            let val item = make_struct [(Clong,toClong left),
                                        (Clong,toClong top),
                                        (Clong,toClong right),
                                        (Clong,toClong bottom)] 
            in
              ( assign (Cstruct [Clong,Clong,Clong,Clong]) 
                       (offset n (Cstruct [Clong,Clong,Clong,Clong]) v) item ;
                pl2a v (n+1) rest ) 
            end
        
            val u = pl2a rectarr 0 rects
            val {left,top,right,bottom} = rectmain 
        
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
          call3 (gdi "ExtCreateRegion")
                 (POINTER,INT,POINTER) (cHRGN)
                 (address xform,struct_size,address rgndata)
        end
*)      
(*      fun GetRegionData h =
        let
          val bufsize = call3 (gdi "GetRegionData")
                            (cHRGN,LONG,POINTER) (LONG)
                            (h,0,toCint 0)
        
          val rgndata = alloc 1 (Cstruct [Clong,Clong,Clong,Clong,
                                          Clong,Clong,Clong,Clong,Cvoid])
        
          val res =  call3 (gdi "GetRegionData")
                            (cHRGN,LONG,POINTER) (LONG)
                            (h,bufsize,address rgndata)
        in
          "not implemented"
        end 
*)
        (*
        Other Region Functions
        CreateEllipticRgnIndirect  
        CreatePolyPolygonRgn  
        CreateRectRgnIndirect  
        ExtCreateRegion  
        GetRegionData  
        *)

    end
end;
