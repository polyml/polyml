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

structure Transform:
  sig
    type HDC (*= Base.HDC*)
    type HWND (*= Base.HWND*)
    type POINT = { x: int, y: int }
    type SIZE = { cx: int, cy: int }
    datatype Fraction = Fraction of {num: int, denom: int}
    
    datatype
      MapMode =
          MM_ANISOTROPIC
        | MM_HIENGLISH
        | MM_HIMETRIC
        | MM_ISOTROPIC
        | MM_LOENGLISH
        | MM_LOMETRIC
        | MM_TEXT
        | MM_TWIPS
    val MM_MAX : MapMode
    val MM_MAX_FIXEDSCALE : MapMode
    val MM_MIN : MapMode

    type XForm = { m11: real, m12: real, m21: real, m22: real, dx: real, dy: real }

    type XFormType
    val MWT_IDENTITY : XFormType
    val MWT_LEFTMULTIPLY : XFormType
    val MWT_MAX : XFormType
    val MWT_MIN : XFormType
    val MWT_RIGHTMULTIPLY : XFormType

    type GraphicsMode
    val GM_ADVANCED : GraphicsMode
    val GM_COMPATIBLE : GraphicsMode
    val GM_ERROR : GraphicsMode

    val ClientToScreen : HWND * POINT -> POINT
    val CombineTransform : XForm * XForm -> XForm
    val DPtoLP : HDC * POINT list -> POINT list
    val GetCurrentPositionEx : HDC -> POINT
    val GetGraphicsMode : HDC -> GraphicsMode
    val GetMapMode : HDC -> MapMode
    val GetViewportExtEx : HDC -> SIZE
    val GetViewportOrgEx : HDC -> POINT
    val GetWindowExtEx : HDC -> SIZE
    val GetWindowOrgEx : HDC -> POINT
    val GetWorldTransform : HDC -> XForm
    val LPtoDP : HDC * POINT list -> POINT list
    val MapWindowPoints : HWND * HWND * POINT list -> POINT list
    val ModifyWorldTransform : HDC * XForm * XFormType -> unit
    val OffsetViewportOrgEx : HDC * int * int -> unit * POINT
    val OffsetWindowOrgEx : HDC * int * int -> unit * POINT
    val ScaleViewportExtEx : HWND * Fraction * Fraction -> SIZE
    val ScaleWindowExtEx : HWND * Fraction * Fraction -> SIZE
    val ScreenToClient : HWND * POINT -> POINT
    val SetGraphicsMode : HDC * GraphicsMode -> GraphicsMode
    val SetMapMode : HDC * MapMode -> MapMode
    val SetViewportExtEx : HDC * int * int -> unit * SIZE
    val SetViewportOrgEx : HDC * int * int -> unit * POINT
    val SetWindowExtEx : HDC * int * int -> unit * SIZE
    val SetWindowOrgEx : HDC * int * int -> unit * POINT
    val SetWorldTransform : HDC * XForm -> unit

  end =
struct
    local
        open CInterface Base GdiBase
        fun callgdi name = call_sym (load_sym (load_lib "gdi32.DLL") name)
    
        fun calluser name = call_sym (load_sym (load_lib "user32.DLL") name)

        fun gdicall_IM name CR (C1,C2) (a1,a2) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = address (to2 a2)
                val res = callgdi name [(ctype1,va1),(Cpointer ctype2,va2)] ctypeR
                val _ : unit = fromR res
            in  from2 (deref va2)
            end

        fun gdicall_WRR name CR (C1,C2,C3) (a2,a3) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = address (alloc 1 ctype1)
                val va2 = address (to2 a2)
                val va3 = address (to3 a3)
                val res = callgdi name [(Cpointer ctype1,va1),(Cpointer ctype2,va2),(Cpointer ctype3,va3)] ctypeR
                val _ : unit = fromR res
            in  from1 (deref va1)
            end

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

        fun gdicall_IRI name CR (C1,C2,C3) (a1,a2,a3) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = address (to2 a2)
                val va3 = to3 a3
                val res = callgdi name [(ctype1,va1),(Cpointer ctype2,va2),(ctype3,va3)] ctypeR
            in  (fromR res)
            end

        fun gdicall_IIIW name CR (C1,C2,C3,C4) (a1,a2,a3) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = address (alloc 1 ctype4)
                val res = callgdi name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(Cpointer ctype4,va4)] ctypeR
            in  (fromR res,from4 (deref va4))
            end

        fun usercall_IM name CR (C1,C2) (a1,a2) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = address (to2 a2)
                val res = calluser name [(ctype1,va1),(Cpointer ctype2,va2)] ctypeR
                val _ : unit = fromR res
            in  from2 (deref va2)
            end

        fun gdicall_IR name CR (C1,C2) (a1,a2) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = address (to2 a2)
                val res = callgdi name [(ctype1,va1),(Cpointer ctype2,va2)] ctypeR
            in  (fromR res)
            end
        val XOFFSET = INT: int Conversion
        val YOFFSET = INT: int Conversion
    in
        type HDC = Base.HDC and HWND = Base.HWND
        type POINT = POINT and SIZE = SIZE

        open GdiBase 

        (* COORDINATE SPACES AND TRANSFORMATIONS *)
        local
            datatype GraphicsMode =
            W of int
        in
            type GraphicsMode = GraphicsMode
            val GRAPHICSMODE = absConversion {abs = W, rep = fn W n => n} INT

            val GM_ERROR (* ???? *)                          = W 0
            val GM_COMPATIBLE                                = W (1)
            val GM_ADVANCED                                  = W (2)
        end


        type XForm = { m11: real, m12: real, m21: real, m22: real, dx: real, dy: real }

        local
            fun breakXForm {m11,m12,m21,m22,dx,dy} = (m11,m12,m21,m22,dx,dy)
            fun mkXForm (m11,m12,m21,m22,dx,dy) = {m11=m11,m12=m12,m21=m21,m22=m22,dx=dx,dy=dy}
        in
         val XFORM = absConversion {abs=mkXForm,
                                    rep=breakXForm} 
                                    (STRUCT6 (FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT))
            
        end

        local
            datatype XFormType =
            W of int
        in
            type XFormType = XFormType
            val XFORMTYPE = absConversion {abs = W, rep = fn W n => n} INT
        
            val MWT_IDENTITY                                 = W (1)
            val MWT_LEFTMULTIPLY                             = W (2)
            val MWT_RIGHTMULTIPLY                            = W (3)
            val MWT_MIN                                      = MWT_IDENTITY
            val MWT_MAX                                      = MWT_RIGHTMULTIPLY
        end

        val ClientToScreen             = gdicall_IM "ClientToScreen" (SUCCESSSTATE "ClientToScreen") (HWND,POINT)
        val CombineTransform           = gdicall_WRR "CombineTransform" (SUCCESSSTATE "CombineTransform") (XFORM,XFORM,XFORM)
        val GetCurrentPositionEx       = gdicall_IW "GetCurrentPositionEx" (SUCCESSSTATE "GetCurrentPositionEx") (HDC,POINT)
        val GetViewportExtEx           = gdicall_IW "GetViewportExtEx" (SUCCESSSTATE "GetViewportExtEx") (HDC,SIZE)
        val GetViewportOrgEx           = gdicall_IW "GetViewportOrgEx" (SUCCESSSTATE "GetViewportOrgEx") (HDC,POINT)
        val GetWindowExtEx             = gdicall_IW "GetWindowExtEx" (SUCCESSSTATE "GetWindowExtEx") (HDC,SIZE)
        val GetWindowOrgEx             = gdicall_IW "GetWindowOrgEx" (SUCCESSSTATE "GetWindowOrgEx") (HDC,POINT)
        val GetWorldTransform          = gdicall_IW "GetWorldTransform" (SUCCESSSTATE "GetWorldTransform") (HDC,XFORM)
        val ModifyWorldTransform       = gdicall_IRI "ModifyWorldTransform" (SUCCESSSTATE "GetWorldTransform") (HDC,XFORM,XFORMTYPE)
        val OffsetViewportOrgEx        = gdicall_IIIW "OffsetViewportOrgEx" (SUCCESSSTATE "OffsetViewportOrgEx") (HDC,XOFFSET,YOFFSET,POINT)
        val OffsetWindowOrgEx          = gdicall_IIIW "OffsetWindowOrgEx" (SUCCESSSTATE "OffsetWindowOrgEx") (HDC,XOFFSET,YOFFSET,POINT)
        val ScreenToClient             = usercall_IM "ScreenToClient" (SUCCESSSTATE "ScreenToClient") (HWND,POINT)
        val SetViewportExtEx           = gdicall_IIIW "SetViewportExtEx" (SUCCESSSTATE "SetViewportExtEx") (HDC,INT,INT,SIZE)
        val SetViewportOrgEx           = gdicall_IIIW "SetViewportOrgEx" (SUCCESSSTATE "SetViewportOrgEx") (HDC,INT,INT,POINT)
        val SetWindowExtEx             = gdicall_IIIW "SetWindowExtEx" (SUCCESSSTATE "SetWindowExtEx") (HDC,INT,INT,SIZE)
        val SetWindowOrgEx             = gdicall_IIIW "SetWindowOrgEx" (SUCCESSSTATE "SetWindowOrgEx") (HDC,INT,INT,POINT)
        val SetWorldTransform          = gdicall_IR "SetWorldTransform" (SUCCESSSTATE "SetWorldTransform") (HDC,XFORM)
        val GetMapMode                 = call1(gdi "GetMapMode") (HDC) MAPMODE
        val SetMapMode                 = call2(gdi "SetMapMode") (HDC,MAPMODE) MAPMODE
        (* Should check the result is non-zero. *)
        val GetGraphicsMode            = call1 (gdi "GetGraphicsMode") (HDC) GRAPHICSMODE
        val SetGraphicsMode            = call2 (gdi "SetGraphicsMode") (HDC, GRAPHICSMODE) GRAPHICSMODE

        datatype Fraction = Fraction of {num:int, denom:int}

        fun ScaleViewportExtEx (h,Fraction{num=n1,denom=d1},Fraction{num=n2,denom=d2}) =
        let
            val (fromSize, _, sizeStruct) = breakConversion SIZE
            val sv = alloc 1 sizeStruct
            val _ = call6 (gdi "ScaleViewportExtEx")
                            (HWND,INT,INT,INT,INT,POINTER) (SUCCESSSTATE "ScaleViewportExtEx")
                            (h,n1,d1,n2,d2,address sv) 
        in
            fromSize sv
        end
        
        fun ScaleWindowExtEx (h,Fraction{num=n1,denom=d1},Fraction{num=n2,denom=d2}) =
        let
            val (fromSize, _, sizeStruct) = breakConversion SIZE
            val sv = alloc 1 sizeStruct
            val _ = call6 (gdi "ScaleWindowExtEx")
                            (HWND,INT,INT,INT,INT,POINTER) (SUCCESSSTATE "ScaleWindowExtEx")
                            (h,n1,d1,n2,d2,address sv)
        in
            fromSize sv
        end
        
        fun DPtoLP (h,pts) = 
        let val count = List.length pts
            val (fromPt, toPt, ptStruct) = breakConversion POINT
            val ptarr = alloc count ptStruct
            val _: int =
                List.foldl (fn (p, n) => (assign ptStruct (offset n ptStruct ptarr) (toPt p); n+1)) 0 pts
            val _:unit = call3 (gdi "DPtoLP") (HDC,POINTER,INT) (SUCCESSSTATE "DPtoLP")
                      (h, address ptarr, count) 
        in
            List.tabulate(count, fn i => fromPt (offset i ptStruct ptarr))
        end
        
        fun LPtoDP (h,pts) = 
        let val count = List.length pts
            val (fromPt, toPt, ptStruct) = breakConversion POINT
            val ptarr = alloc count ptStruct
            val _: int =
                List.foldl (fn (p, n) => (assign ptStruct (offset n ptStruct ptarr) (toPt p); n+1)) 0 pts
            val _:unit = call3 (gdi "LPtoDP") (HDC,POINTER,INT) (SUCCESSSTATE "LPtoDP")
                      (h, address ptarr, count) 
        in
            List.tabulate(count, fn i => fromPt (offset i ptStruct ptarr))
        end

        fun MapWindowPoints (h1,h2,pts) = 
        let
            val count = List.length pts
            val (fromPt, toPt, ptStruct) = breakConversion POINT
            val ptarr = alloc count ptStruct
            val _: int =
                List.foldl (fn (p, n) => (assign ptStruct (offset n ptStruct ptarr) (toPt p); n+1)) 0 pts

            (* The result is the bits added in each direction to make the mapping or is
               zero if there is an error.  Isn't it possible that if the two windows were
               exactly aligned the result could be zero? *)
            val res = call4 (user "MapWindowPoints")
                            (HWND,HWND,POINTER,INT) (INT)
                            (h1,h2,address ptarr,count)
        in
            List.tabulate(count, fn i => fromPt (offset i ptStruct ptarr))
        end
    end
end;
