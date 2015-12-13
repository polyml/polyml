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
    val OffsetViewportOrgEx : HDC * int * int -> POINT
    val OffsetWindowOrgEx : HDC * int * int -> POINT
    val ScaleViewportExtEx : HWND * Fraction * Fraction -> SIZE
    val ScaleWindowExtEx : HWND * Fraction * Fraction -> SIZE
    val ScreenToClient : HWND * POINT -> POINT
    val SetGraphicsMode : HDC * GraphicsMode -> GraphicsMode
    val SetMapMode : HDC * MapMode -> MapMode
    val SetViewportExtEx : HDC * int * int -> SIZE
    val SetViewportOrgEx : HDC * int * int -> POINT
    val SetWindowExtEx : HDC * int * int -> SIZE
    val SetWindowOrgEx : HDC * int * int -> POINT
    val SetWorldTransform : HDC * XForm -> unit

  end =
struct
    local
        open Foreign Base GdiBase
    in
        type HDC = Base.HDC and HWND = Base.HWND
        type POINT = POINT and SIZE = SIZE

        open GdiBase 

        (* COORDINATE SPACES AND TRANSFORMATIONS *)
        local
            datatype GraphicsMode = W of int
        in
            type GraphicsMode = GraphicsMode
            val GRAPHICSMODE = absConversion {abs = W, rep = fn W n => n} cInt

            val GM_ERROR (* ???? *)                          = W 0
            val GM_COMPATIBLE                                = W (1)
            val GM_ADVANCED                                  = W (2)
        end


        (* An XFORM is a struct of six floats.  Wrap this as an ML record for clarity *)
        type XForm = { m11: real, m12: real, m21: real, m22: real, dx: real, dy: real }

        local
            fun breakXForm {m11,m12,m21,m22,dx,dy} = (m11,m12,m21,m22,dx,dy)
            fun mkXForm (m11,m12,m21,m22,dx,dy) = {m11=m11,m12=m12,m21=m21,m22=m22,dx=dx,dy=dy}
        in
            val XFORM =
                absConversion {abs=mkXForm, rep=breakXForm} 
                    (cStruct6 (cFloat,cFloat,cFloat,cFloat,cFloat,cFloat))
        end

        local
            datatype XFormType = W of int
        in
            type XFormType = XFormType
            val XFORMTYPE = absConversion {abs = W, rep = fn W n => n} cDWORD
        
            val MWT_IDENTITY                                 = W (1)
            val MWT_LEFTMULTIPLY                             = W (2)
            val MWT_RIGHTMULTIPLY                            = W (3)
            val MWT_MIN                                      = MWT_IDENTITY
            val MWT_MAX                                      = MWT_RIGHTMULTIPLY
        end

        datatype Fraction = Fraction of {num:int, denom:int}

        local
            val clientToScreen              = winCall2(user "ClientToScreen") (cHWND, cStar cPoint) (successState "ClientToScreen")
            val combineTransform            = winCall3(gdi "CombineTransform") (cStar XFORM, cConstStar XFORM, cConstStar XFORM) (successState "CombineTransform")
            val getCurrentPositionEx        = winCall2(gdi "GetCurrentPositionEx") (cHDC, cStar cPoint) (successState "GetCurrentPositionEx")
            val getViewportExtEx            = winCall2(gdi "GetViewportExtEx") (cHDC, cStar cSize)  (successState "GetViewportExtEx")
            val getViewportOrgEx            = winCall2(gdi "GetViewportOrgEx") (cHDC, cStar cPoint) (successState "GetViewportOrgEx")
            val getWindowExtEx              = winCall2(gdi "GetWindowExtEx") (cHDC, cStar cSize)  (successState "GetWindowExtEx")
            val getWindowOrgEx              = winCall2(gdi "GetWindowOrgEx") (cHDC, cStar cPoint) (successState "GetWindowOrgEx")
            val getWorldTransform           = winCall2(gdi "GetWorldTransform") (cHDC, cStar XFORM) (successState "GetWorldTransform")
            val offsetViewportOrgEx         = winCall4(gdi "OffsetViewportOrgEx") (cHDC, cInt, cInt, cStar cPoint) (successState "OffsetViewportOrgEx")
            val offsetWindowOrgEx           = winCall4(gdi "OffsetWindowOrgEx") (cHDC, cInt, cInt, cStar cPoint) (successState "OffsetWindowOrgEx")
            val screenToClient              = winCall2(user "ScreenToClient") (cHWND, cStar cPoint) (successState "ScreenToClient")
            val setViewportExtEx            = winCall4(gdi "SetViewportExtEx") (cHDC, cInt, cInt, cStar cSize) (successState "SetViewportExtEx")
            val setViewportOrgEx            = winCall4(gdi "SetViewportOrgEx") (cHDC, cInt, cInt, cStar cPoint) (successState "SetViewportOrgEx")
            val setWindowExtEx              = winCall4(gdi "SetWindowExtEx") (cHDC, cInt, cInt, cStar cSize) (successState "SetWindowExtEx")
            val setWindowOrgEx              = winCall4(gdi "SetWindowOrgEx") (cHDC, cInt, cInt, cStar cPoint) (successState "SetWindowOrgEx")
            val scaleViewportExtEx =
                winCall6 (gdi "ScaleViewportExtEx") (cHWND,cInt,cInt,cInt,cInt,cStar cSize) (successState "ScaleViewportExtEx")
            val scaleWindowExtEx =
                winCall6 (gdi "ScaleWindowExtEx") (cHWND,cInt,cInt,cInt,cInt,cStar cSize) (successState "ScaleWindowExtEx")

            val zeroXFORM: XForm = { m11=0.0, m12=0.0, m21=0.0, m22=0.0, dx=0.0, dy=0.0 }
            val zeroPoint: POINT = { x = 0, y = 0 }
            val zeroSize: SIZE = { cx = 0, cy = 0 }
            
        in
            fun ClientToScreen(w, p)        = let val r = ref p in clientToScreen(w, r); !r end
            and CombineTransform(a, b)      = let val r = ref zeroXFORM in combineTransform(r, a, b); ! r end
            and GetCurrentPositionEx hdc    = let val p = ref zeroPoint in getCurrentPositionEx(hdc, p); !p end
            and GetViewportExtEx hdc        = let val s = ref zeroSize in getViewportExtEx(hdc, s); !s end
            and GetViewportOrgEx  hdc       = let val p = ref zeroPoint in getViewportOrgEx(hdc, p); !p end
            and GetWindowExtEx hdc          = let val s = ref zeroSize in getWindowExtEx(hdc, s); !s end
            and GetWindowOrgEx hdc          = let val p = ref zeroPoint in getWindowOrgEx(hdc, p); !p end
            and GetWorldTransform hdc       = let val r = ref zeroXFORM in getWorldTransform(hdc, r); !r end
            and OffsetViewportOrgEx (hdc, x, y) =
                let val p = ref zeroPoint in offsetViewportOrgEx(hdc, x, y, p); !p end
            and OffsetWindowOrgEx (hdc, x, y) =
                let val p = ref zeroPoint in offsetWindowOrgEx(hdc, x, y, p); !p end
            and ScreenToClient(hw, p)       = let val r = ref p in screenToClient(hw, r); !r end
            and SetViewportExtEx (hdc, x, y) =
                let val p = ref zeroSize in setViewportExtEx(hdc, x, y, p); !p end
            and SetViewportOrgEx (hdc, x, y) =
                let val p = ref zeroPoint in setViewportOrgEx(hdc, x, y, p); !p end
            and SetWindowExtEx (hdc, x, y) =
                let val p = ref zeroSize in setWindowExtEx(hdc, x, y, p); !p end
            and SetWindowOrgEx (hdc, x, y) =
                let val p = ref zeroPoint in setWindowOrgEx(hdc, x, y, p); !p end
            and ScaleViewportExtEx (h,Fraction{num=n1,denom=d1},Fraction{num=n2,denom=d2}) =
                let val p = ref zeroSize in scaleViewportExtEx(h,n1,d1,n2,d2,p); !p end
            and ScaleWindowExtEx (h,Fraction{num=n1,denom=d1},Fraction{num=n2,denom=d2}) =
                let val p = ref zeroSize in scaleWindowExtEx(h,n1,d1,n2,d2,p); !p end
        end
        
        val ModifyWorldTransform    = winCall3(gdi "ModifyWorldTransform")  (cHDC, cConstStar XFORM, XFORMTYPE) (successState "ModifyWorldTransform")
        val SetWorldTransform       = winCall2(gdi "SetWorldTransform") (cHDC, cConstStar XFORM) (successState "SetWorldTransform")

        
        val GetMapMode                 = winCall1(gdi "GetMapMode") (cHDC) cMAPMODE
        val SetMapMode                 = winCall2(gdi "SetMapMode") (cHDC,cMAPMODE) cMAPMODE
        (* Should check the result is non-zero. *)
        val GetGraphicsMode            = winCall1 (gdi "GetGraphicsMode") (cHDC) GRAPHICSMODE
        val SetGraphicsMode            = winCall2 (gdi "SetGraphicsMode") (cHDC, GRAPHICSMODE) GRAPHICSMODE

        local
            val dPtoLP = winCall3 (gdi "DPtoLP") (cHDC,cPointer,cInt) (successState "DPtoLP")
            and lPtoDP = winCall3 (gdi "LPtoDP") (cHDC,cPointer,cInt) (successState "LPtoDP")
            (* The result is the bits added in each direction to make the mapping or is
               zero if there is an error.  The caller is supposed to call SetLastError and
               check GetLastError because the result could legitimately be zero.  *)
            and mapWindowPoints = winCall4 (user "MapWindowPoints") (cHWND,cHWND,cPointer,cInt) cInt
            
            val {load=fromPt, store=toPt, ctype={size=sizePt, ...}, ...} = breakConversion cPoint

            fun mapPts call pts =
            let
                val count = List.length pts
                open Memory
                infix 6 ++
                val mem = malloc(Word.fromInt count * sizePt)
                val _ = List.foldl (fn (p,n) => (ignore(toPt(n, p)); n ++ sizePt)) mem pts
                val _ = call(mem, count) handle ex => (free mem; raise ex)
            in
                List.tabulate(count, fn i => fromPt(mem ++ (Word.fromInt i * sizePt)))
                    before free mem
            end
        in
            fun DPtoLP(h,pts) = mapPts(fn (mem, count) => dPtoLP(h, mem, count)) pts
            and LPtoDP(h,pts) = mapPts(fn (mem, count) => lPtoDP(h, mem, count)) pts
            and MapWindowPoints (h1,h2,pts) = mapPts(fn (mem, count) => mapWindowPoints(h1, h2, mem, count)) pts
         end
    end
end;
