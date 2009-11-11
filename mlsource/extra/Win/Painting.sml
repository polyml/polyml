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

structure Painting :
  sig
    type HWND and HDC and COLORREF and HRGN

    type RECT = { top: int, left: int, bottom: int, right: int }

    type BinaryRasterOperation
    val R2_BLACK : BinaryRasterOperation
    val R2_COPYPEN : BinaryRasterOperation
    val R2_MASKNOTPEN : BinaryRasterOperation
    val R2_MASKPEN : BinaryRasterOperation
    val R2_MASKPENNOT : BinaryRasterOperation
    val R2_MERGENOTPEN : BinaryRasterOperation
    val R2_MERGEPEN : BinaryRasterOperation
    val R2_MERGEPENNOT : BinaryRasterOperation
    val R2_NOP : BinaryRasterOperation
    val R2_NOT : BinaryRasterOperation
    val R2_NOTCOPYPEN : BinaryRasterOperation
    val R2_NOTMASKPEN : BinaryRasterOperation
    val R2_NOTMERGEPEN : BinaryRasterOperation
    val R2_NOTXORPEN : BinaryRasterOperation
    val R2_WHITE : BinaryRasterOperation
    val R2_XORPEN : BinaryRasterOperation

    type PAINTSTRUCT =
        { hdc: HDC, erase: bool, paint: RECT, private: Word8Vector.vector }

    val BeginPaint : HWND -> HDC * PAINTSTRUCT
    val EndPaint : HWND * PAINTSTRUCT -> unit
    val GdiFlush : unit -> unit
    val GdiGetBatchLimit : unit -> int
    val GdiSetBatchLimit : int -> int
    val GetBkColor : HDC -> COLORREF
    val GetROP2 : HDC -> BinaryRasterOperation
    val GetUpdateRect : HWND * bool -> RECT option
    val GetUpdateRgn : HWND * HRGN * bool -> Region.ResultRegion
    val GetWindowDC : HWND -> HDC
    val InvalidateRgn : HWND * HRGN * bool -> unit
    val SetBkColor : HDC * COLORREF -> COLORREF
    val SetROP2 : HDC * BinaryRasterOperation -> BinaryRasterOperation
    val WindowFromDC : HDC -> HWND
    val InvalidateRect: HWND * RECT * bool -> unit
  end =
struct
    local
        open CInterface Base GdiBase

        fun checkDC c = (checkResult(not(isHdcNull c)); c)
    in
        type ResultRegion = Region.ResultRegion
        type HDC = HDC and HRGN = HRGN and HWND = HWND
        type RECT = RECT and COLORREF = COLORREF

        (* GetROP2 and SetROP2. *)
        local
            datatype BinaryRasterOperation =
            W of int
        in
            type BinaryRasterOperation = BinaryRasterOperation
            val BINARYRASTEROPERATION = absConversion {abs = W, rep = fn W n => n} INT
        
            val R2_BLACK                                     = W (1 (* 0 *))
            val R2_NOTMERGEPEN                               = W (2 (* DPon *))
            val R2_MASKNOTPEN                                = W (3 (* DPna *))
            val R2_NOTCOPYPEN                                = W (4 (* PN *))
            val R2_MASKPENNOT                                = W (5 (* PDna *))
            val R2_NOT                                       = W (6 (* Dn *))
            val R2_XORPEN                                    = W (7 (* DPx *))
            val R2_NOTMASKPEN                                = W (8 (* DPan *))
            val R2_MASKPEN                                   = W (9 (* DPa *))
            val R2_NOTXORPEN                                 = W (10 (* DPxn *))
            val R2_NOP                                       = W (11 (* D *))
            val R2_MERGENOTPEN                               = W (12 (* DPno *))
            val R2_COPYPEN                                   = W (13 (* P *))
            val R2_MERGEPENNOT                               = W (14 (* PDno *))
            val R2_MERGEPEN                                  = W (15 (* DPo *))
            val R2_WHITE                                     = W (16 (* 1 *))
        end

        val GdiFlush               = call0 (gdi "GdiFlush") () (SUCCESSSTATE "GdiFlush")
        val GdiGetBatchLimit       = call0 (gdi "GdiGetBatchLimit") () INT
        val GdiSetBatchLimit       = call1 (gdi "GdiSetBatchLimit") (INT) INT
        val GetBkColor             = call1 (gdi "GetBkColor") (HDC) COLORREF
        val GetROP2                = call1(user "GetROP2") (HDC) BINARYRASTEROPERATION
        val GetUpdateRgn           = call3(user "GetUpdateRgn") (HWND,HRGN,BOOL) RESULTREGION
        val GetWindowDC            = call1(user "GetWindowDC") (HWND) HDC
        val InvalidateRgn          = call3(user "InvalidateRgn") (HWND,HRGN,BOOL) (SUCCESSSTATE "InvalidateRgn")
        val SetBkColor             = call2 (gdi "SetBkColor") (HDC, COLORREF) COLORREF
        val WindowFromDC           = call1(user "WindowFromDC") (HDC) HWND
        val SetROP2                = call2(user "SetROP2") (HDC, BINARYRASTEROPERATION) BINARYRASTEROPERATION

        fun GetUpdateRect (hw: HWND, erase: bool): RECT option =
        let
            val (from,_,rtype) = breakConversion RECT 
            val va = address (alloc 1 rtype)
            (* If the update area is empty the result is zero. *)
            val res = call3 (user "GetUpdateRect") (HWND, POINTER, BOOL) BOOL (hw, va, erase)
        in
            if res then SOME(from(deref va)) else NONE
        end

        type PAINTSTRUCT =
            { hdc: HDC, erase: bool, paint: RECT, private: Word8Vector.vector }

        local
            val PAINTSTRUCT = STRUCT4(HDC, BOOL, RECT, BYTEARRAY 40)
            val (fromPS, toPS, psStr) = breakConversion PAINTSTRUCT

            fun fromCPS(b: vol): PAINTSTRUCT =
            let
                val (hdc, erase, paint, private) = fromPS b
            in
                {hdc = hdc, erase = erase, paint = paint, private = private}
            end

            fun toCPS({hdc, erase, paint, private}: PAINTSTRUCT): vol =
                toPS(hdc, erase, paint, private)
    
            val beginPaint = call2 (user "BeginPaint") (HWND, POINTER) HDC
            val endPaint = call2 (user "EndPaint") (HWND, POINTER) VOID

        in
            fun BeginPaint(hwnd: HWND): HDC * PAINTSTRUCT =
            let
                val b = alloc 1 psStr
                val hdc = checkDC (beginPaint (hwnd, address b))
            in
                (hdc, fromCPS b)
            end
            and EndPaint(hwnd: HWND, ps: PAINTSTRUCT): unit = endPaint(hwnd, address(toCPS ps))
        end
        
        val InvalidateRect =
            checkResult o
            (call3 (user "InvalidateRect") (HWND, POINTERTO RECT, BOOL) BOOL)
        (*
            Other painting and drawing functions:
                DrawAnimatedRects  
                DrawCaption  
                DrawEdge  
                DrawFocusRect  
                DrawFrameControl  
                DrawState  
                DrawStateProc  
                ExcludeUpdateRgn  
                GetBkMode  
                GetBoundsRect  
                GetWindowRgn  
                GrayString  
                LockWindowUpdate  
                OutputProc  
                PaintDesktop  
                RedrawWindow  
                SetBkMode  
                SetBoundsRect  
                SetWindowRgn  
                UpdateWindow  
                ValidateRect  
                ValidateRgn  
        *)

    end
end;
