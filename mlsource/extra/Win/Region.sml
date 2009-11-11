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
    val CreatePolygonRgn : POINT list * PolyFillMode -> HPEN
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
    val SetRectRgn : HRGN * RECT -> bool

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
        val XCOORD = INT : int Conversion
        val YCOORD = INT: int Conversion
        val XOFFSET = INT : int Conversion
        val YOFFSET = INT: int Conversion
        val HEIGHT = INT: int Conversion
        val WIDTH = INT: int Conversion
    in
        type HRGN = Base.HRGN and HBRUSH = Base.HBRUSH and HDC = Base.HDC
        and HPEN = HPEN and RECT = RECT and POINT = POINT

        open GdiBase

        local
            datatype PolyFillMode =
            W of int
        in
            type PolyFillMode = PolyFillMode
            val POLYFILLMODE = absConversion {abs = W, rep = fn W n => n} INT
        
            val ALTERNATE                                    = W (1)
            val WINDING                                      = W (2)
        end


        val CombineRgn                 = call4(gdi "CombineRgn") (HRGN,HRGN,HRGN,REGIONOPERATION) RESULTREGION 
        val EqualRgn                   = call2(gdi "EqualRgn") (HRGN,HRGN) BOOL
        val FillRgn                    = call3(gdi "FillRgn") (HDC,HRGN,HBRUSH) (SUCCESSSTATE "FillRgn")
        val FrameRgn                   = call5(gdi "FrameRgn") (HDC,HRGN,HBRUSH,WIDTH,HEIGHT) (SUCCESSSTATE "FrameRgn")
        val GetPolyFillMode            = call1(gdi "GetPolyFillMode") (HDC) POLYFILLMODE
        val InvertRgn                  = call2(gdi "InvertRgn") (HDC,HRGN) (SUCCESSSTATE "InvertRgn")
        val OffsetRgn                  = call3(gdi "OffsetRgn") (HRGN,XOFFSET,YOFFSET) RESULTREGION
        val PaintRgn                   = call2(gdi "PaintRgn") (HDC,HRGN) (SUCCESSSTATE "PaintRgn")
        val PtInRegion                 = call3(gdi "PtInRegion") (HRGN,XCOORD,YCOORD) BOOL
        val GetRgnBox                  = gdicall_IW "GetRgnBox" (SUCCESSSTATE "GetRgnBox") (HRGN,RECT)
        val RectInRegion               = call2(gdi "RectInRegion") (HRGN,RECT) BOOL
        val SetPolyFillMode            = call2(gdi "SetPolyFillMode") (HDC,POLYFILLMODE) POLYFILLMODE

        fun SetRectRgn (h, ({ left, top, right, bottom }: RECT)) =
           call5 (gdi "SetRectRgn") (HRGN,LONG,LONG,LONG,LONG) (BOOL) (h,left,top,right,bottom)

        fun CreateEllipticRgn ({left,top,right,bottom}: RECT) =
            call4 (gdi "CreateEllipticRgn")
                  (LONG,LONG,LONG,LONG) (HRGN)
                  (left,top,right,bottom)
        
        fun CreateRectRgn ({left,top,right,bottom}: RECT) = 
            call4 (gdi "CreateRectRgn")
                  (LONG,LONG,LONG,LONG) (HRGN)
                  (left,top,right,bottom)
        
        fun CreateRoundRectRgn ({left,top,right,bottom}: RECT,w,h) =
            call6 (gdi "CreateRoundRectRgn")
                  (LONG,LONG,LONG,LONG,WIDTH,HEIGHT) (HRGN)
                  (left,top,right,bottom,w,h)
        
        fun CreatePolygonRgn (pts: POINT list,fmode) = 
        let val count = List.length pts
            val ptarr = alloc count (Cstruct [Clong,Clong])
        
            fun pl2a v n [] = () 
            |   pl2a v n ({x,y} :: rest) = 
            let val item = make_struct [(Clong,toClong x),
                                       (Clong,toClong y)] 
            in
              ( assign (Cstruct [Clong,Clong]) (offset n (Cstruct [Clong,Clong]) v) item ;
                pl2a v (n+1) rest ) 
            end
        
            val u = pl2a ptarr 0 pts
        in
           call3 (gdi "CreatePolygonRgn")
                 (POINTER,INT,POLYFILLMODE) (HPEN)
                 (address ptarr,count,fmode)
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
                 (POINTER,INT,POINTER) (HRGN)
                 (address xform,struct_size,address rgndata)
        end
*)      
(*      fun GetRegionData h =
        let
          val bufsize = call3 (gdi "GetRegionData")
                            (HRGN,LONG,POINTER) (LONG)
                            (h,0,toCint 0)
        
          val rgndata = alloc 1 (Cstruct [Clong,Clong,Clong,Clong,
                                          Clong,Clong,Clong,Clong,Cvoid])
        
          val res =  call3 (gdi "GetRegionData")
                            (HRGN,LONG,POINTER) (LONG)
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
