(*
    Copyright (c) 2001-7, 2015
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

structure Bitmap:
  sig
    type HBITMAP and HDC
    type COLORREF = Color.COLORREF
    type RECT = { top: int, left: int, bottom: int, right: int }
    type SIZE = { cx: int, cy: int }
    datatype BitmapCompression = BI_BITFIELDS | BI_RGB | BI_RLE4 | BI_RLE8
    datatype FloodFillMode = FLOODFILLBORDER | FLOODFILLSURFACE

    type BITMAP =
        { width: int, height: int, widthBytes: int, planes: int, bitsPerPixel: int,
          bits: Word8Vector.vector option }

    type StretchMode
    val BLACKONWHITE : StretchMode
    val COLORONCOLOR : StretchMode
    val HALFTONE : StretchMode
    val MAXSTRETCHBLTMODE : StretchMode
    val WHITEONBLACK : StretchMode

    type RasterOpCode
    val BLACKNESS : RasterOpCode
    val DSTINVERT : RasterOpCode
    val MERGECOPY : RasterOpCode
    val MERGEPAINT : RasterOpCode
    val NOTSRCCOPY : RasterOpCode
    val NOTSRCERASE : RasterOpCode
    val PATCOPY : RasterOpCode
    val PATINVERT : RasterOpCode
    val PATPAINT : RasterOpCode
    val SRCAND : RasterOpCode
    val SRCCOPY : RasterOpCode
    val SRCERASE : RasterOpCode
    val SRCINVERT : RasterOpCode
    val SRCPAINT : RasterOpCode
    val WHITENESS : RasterOpCode

    val BitBlt : HDC * int * int * int * int * HDC * int * int * RasterOpCode -> unit
    val CreateBitmap :
       {bits: Word8Vector.vector option, width: int, height: int,
         planes: int, bitsPerPixel: int} -> HBITMAP
    val CreateBitmapIndirect : BITMAP -> HBITMAP
    val CreateCompatibleBitmap : HDC * int * int -> HBITMAP
    val ExtFloodFill : HDC * int * int * COLORREF * FloodFillMode -> unit
    val GetBitmapBits : HBITMAP * int -> Word8Vector.vector
    val GetBitmapDimensionEx : HBITMAP -> SIZE
    val GetPixel : HDC * int * int -> COLORREF
    val GetStretchBltMode : HDC -> StretchMode

    type QuaternaryRop
    val MAKEROP4 : {back: RasterOpCode, fore: RasterOpCode} -> QuaternaryRop
    val MaskBlt :
       HDC * int * int * int * int * HDC * int * int *
       HBITMAP * int * int * QuaternaryRop -> unit

    (*val PlgBlt : HDC * RECT * HDC * RECT * HBITMAP * int * int -> unit*)
    val SetBitmapBits : HBITMAP * Word8Vector.vector -> unit
    val SetBitmapDimensionEx : HBITMAP * int * int * SIZE -> SIZE
    val SetPixel : HDC * int * int * COLORREF -> COLORREF
    val SetStretchBltMode : HDC * StretchMode -> unit
    val StretchBlt :
       HDC * int * int * int * int * HDC * int * int * int * int * RasterOpCode -> unit

    type BITMAPINFOHEADER =
    {
        width: int, height: int, planes: int, bitsPerPixel: int,
        compression: BitmapCompression, sizeImage: int, xPelsPerM: int, 
        yPelsPerM: int, clrUsed: int, clrImportant: int
    }
    (* ML extension to extract the information from a DIB. *)
    val getBitmapInfoHdr: Word8Vector.vector -> BITMAPINFOHEADER
    val GetDIBits: HDC * HBITMAP * int * int * BITMAPINFOHEADER option -> Word8Vector.vector
    val SetDIBits: HDC * HBITMAP * int * int * Word8Vector.vector -> unit

  end =
struct
    local
        open Foreign Base

        fun checkBitmap c = (checkResult(not(isHgdiObjNull c)); c)
    in
        type HDC = HDC and HBITMAP = HBITMAP
        type COLORREF = Color.COLORREF
        type SIZE = SIZE and RECT = RECT

        open GdiBase

        local
            datatype StretchMode =
            W of int
        in
            type StretchMode = StretchMode
            val STRETCHMODE = absConversion {abs = W, rep = fn W n => n} cInt
        
            val BLACKONWHITE                                 = W (1)
            val WHITEONBLACK                                 = W (2)
            val COLORONCOLOR                                 = W (3)
            val HALFTONE                                     = W (4)
            val MAXSTRETCHBLTMODE                            = W (4)
        end   
        
        (*TYPE: FloodFillMode *)
        datatype FloodFillMode = FLOODFILLBORDER | FLOODFILLSURFACE
        local
            val tab = [
                (FLOODFILLBORDER, 0),
                (FLOODFILLSURFACE, 1)
                ]
            
        in
            val FLOODFILLMODE = tableConversion(tab, NONE) cUint
        end

        val ExtFloodFill =
            winCall5 (gdi "ExtFloodFill") 
                   (cHDC,cInt,cInt,cCOLORREF,FLOODFILLMODE) (successState "ExtFloodFill")

        val GetPixel = winCall3 (gdi "GetPixel") (cHDC,cInt,cInt) cCOLORREF
        val SetPixel = winCall4 (gdi "SetPixel") (cHDC,cInt,cInt, cCOLORREF) cCOLORREF
        val BitBlt = winCall9 (gdi  "BitBlt") (cHDC,cInt,cInt,cInt,cInt,cHDC,cInt,cInt,cRASTEROPCODE)
                (successState "BitBlt")
                                         

        val CreateCompatibleBitmap     = 
            checkBitmap o
                winCall3 (gdi "CreateCompatibleBitmap") (cHDC,cInt,cInt) cHBITMAP


        val GetStretchBltMode          = winCall1 (gdi "GetStretchBltMode") (cHDC) STRETCHMODE

        (* TODO: The raster op is supposed to be a combined operation for the foreground and
           background. *)
        val MaskBlt = winCall12(gdi "MaskBlt") (cHDC,cInt,cInt,cInt,cInt,cHDC,cInt,cInt,cHBITMAP,cInt,
                                          cInt,cQUATERNARY) (successState "MaskBlt")

        val SetStretchBltMode = winCall2(gdi "SetStretchBltMode") (cHDC,STRETCHMODE) (successState "SetStretchBltMode")

        val StretchBlt =
            winCall11(gdi "StretchBlt") 
                (cHDC,cInt,cInt,cInt,cInt,cHDC,cInt,cInt,cInt,cInt,cRASTEROPCODE) (successState "StretchBlt")

        (* This definitely has the wrong type. *)
        (*val PlgBlt = winCall7 (gdi "PlgBlt")(cHDC,RECT,cHDC,RECT,HBITMAP,XCOORD,YCOORD)
                 (successState "PlgBlt")*)
                                         

        local
            val setBitmapDimensionEx =
                winCall4 (gdi "SetBitmapDimensionEx") (cHBITMAP, cInt, cInt, cStar cSize) (successState "SetBitmapDimensionEx")
        in
            fun SetBitmapDimensionEx(hbm, width, height, s) =
            let
                val r = ref s
            in
                setBitmapDimensionEx(hbm, width, height, r);
                !r
            end
        end
        local
            val getBitmapDimensionEx =
                winCall2 (gdi "GetBitmapDimensionEx") (cHBITMAP, cStar cSize) (successState "SetBitmapDimensionEx")
        in
            fun GetBitmapDimensionEx hbm =
            let
                val r = ref {cx=0, cy=0}
            in
                getBitmapDimensionEx(hbm, r);
                !r
            end
        end

        val CreateBitmapIndirect       =
            checkBitmap o
                winCall1 (gdi "CreateBitmapIndirect") (cConstStar cBITMAP) cHBITMAP

        local
            val cbm = checkBitmap o
                winCall5 (gdi "CreateBitmap") (cInt, cInt, cInt, cInt, cPointer) cHBITMAP
        in
            fun CreateBitmap{width, height, planes, bitsPerPixel, bits} =
            let
                val vec = case bits of NONE => Memory.null | SOME v => toCWord8vec v
                val res = 
                    cbm(width, height, planes, bitsPerPixel, vec)
                        handle ex => (Memory.free vec; raise ex)
            in
                Memory.free vec;
                checkBitmap res
            end
        end
(*
        local
            (* RGBQUAD values are four bytes of blue, green, red and a reserved byte. *)
            val RGBQUAD = cStruct4(cUint8, cUint8, cUint8, cUint8)
            fun from v =
                let val (b, g, r, _) = v in {red = r, blue = b, green = g} end
            fun to {red, green, blue} = (blue, green, red, 0)
        in
            val RGBQUAD = absConversion {rep=to, abs=from} RGBQUAD
        end*)

        (*TYPE: BitmapCompression *)
        datatype BitmapCompression = BI_RGB | BI_RLE8 | BI_RLE4 | BI_BITFIELDS
        
        local
            val tab = [
                (BI_RGB, 0),
                (BI_RLE8, 1),
                (BI_RLE4, 2),
                (BI_BITFIELDS, 3)
            ]
        in
            val (fromComp, toComp) = tableLookup(tab, NONE)
            val BITCOMPRESSION = absConversion {abs = toComp, rep = fromComp} cDWORD
        end

        type BITMAPINFOHEADER =
        {
            width: int, height: int, planes: int, bitsPerPixel: int,
            compression: BitmapCompression, sizeImage: int, xPelsPerM: int, 
            yPelsPerM: int, clrUsed: int, clrImportant: int
        }

        (* Device-independent bitmaps are intended to be used for storing and
           transferring bitmaps.  I've written this code to simplify the process
           of packing and unpacking them.  In particular it takes care of the
           calculating the header size which is generally a bit of a pain.  DCJM. *)
        fun getBitmapInfoHdr(w: Word8Vector.vector): BITMAPINFOHEADER =
        let
            val size = LargeWord.toInt(PackWord32Little.subVec(w, 0))
            (* Check that the size of the structure given by the
               first word is less than the overall size.  There are
               various extended versions of the BITMAPINFOHEADER structure
               but we only look at the fields in the basic one. *)
            val _ =
                if size > Word8Vector.length w
                then raise Fail "Bitmap length field is wrong"
                else ()
            val width = LargeWord.toIntX(PackWord32Little.subVecX(w, 1))
            val height = LargeWord.toIntX(PackWord32Little.subVecX(w, 2))
            val planes = LargeWord.toIntX(PackWord16Little.subVecX(w, 6))
            val bitsPerPixel = LargeWord.toIntX(PackWord16Little.subVecX(w, 7))
            val compression = toComp(LargeWord.toIntX(PackWord32Little.subVecX(w, 4)))
            val sizeImage = LargeWord.toIntX(PackWord32Little.subVecX(w, 5))
            val xPelsPerM = LargeWord.toIntX(PackWord32Little.subVecX(w, 6))
            val yPelsPerM = LargeWord.toIntX(PackWord32Little.subVecX(w, 7))
            val clrUsed = LargeWord.toIntX(PackWord32Little.subVecX(w, 8))
            val clrImportant = LargeWord.toIntX(PackWord32Little.subVecX(w, 9))
        in
            { width = width, height = height, bitsPerPixel = bitsPerPixel,
              planes = planes, compression = compression, sizeImage = sizeImage,
              xPelsPerM = xPelsPerM, yPelsPerM = yPelsPerM, clrUsed = clrUsed,
              clrImportant = clrImportant }
        end

        local
            val DIB_RGB_COLORS =     0
            (*val DIB_PAL_COLORS =     1*)

            val BITMAPINFOHEADER = cStruct11(cDWORD, cLong, cLong, cWORD, cWORD, BITCOMPRESSION,
                cDWORD, cLong, cLong, cDWORD, cDWORD)
            val {load=fromR, store=toR, ctype={size=rtypeSize, ...}} =
                breakConversion BITMAPINFOHEADER

            val getDIBits = winCall7 (gdi "GetDIBits")
                (cHDC, cHBITMAP, cUint, cUint, cPointer, cPointer, cUint) cInt

            val setDIBits = winCall7 (gdi "SetDIBits")
                (cHDC, cHBITMAP, cUint, cUint, cPointer, cPointer, cUint) cInt
            
            val sizeColourEntry = #size LowLevel.cTypeInt (* Should this RGBQUAD? *)
                
        in
            (* This is all a bit messy.  GetDIBits can be used in a number of ways
               to get all or part of the information.  Passing NULL for the "bits"
               argument and setting bitsPerPixel to zero in the BITMAPINFO argument
               simply fills in the BITMAPINFOHEADER.  With bitsPerPixel non-zero it
               builds a colour table on the end of the BITMAPINFO.  With "bits"
               non-NULL it builds the colour table and creates the bitmap.

               If NONE is given as the header it returns a vector containing
               only the header, allowing getBitmapInfoHdr to be used to unpack it.
               Otherwise it uses the information in the supplied header to
               get the bitmap.  It ignores the passed in sizeImage because that
               may be wrong. *)
            fun GetDIBits(hdc: HDC, hb: HBITMAP, startScan, scanLines, NONE) =
                let
                    (* Allocate a vector for the result and set the length field
                       and bitsPerPixel.  The others don't matter. *)
                    open Memory
                    val v = malloc rtypeSize
                    val _ = toR(v, (Word.toInt rtypeSize, 0, 0, 0, 0, BI_RGB, 0, 0, 0, 0, 0))
                    val res =
                        getDIBits(hdc, hb, startScan, scanLines, Memory.null, v, DIB_RGB_COLORS)
                            handle ex => (free v; raise ex)
                in
                    checkResult(res <> 0) handle ex => (free v; raise ex);
                    fromCWord8vec(v, Word.toInt rtypeSize) before free v
                end

             |  GetDIBits(hdc: HDC, hb: HBITMAP, startScan, scanLines,
                    SOME {width, height, planes, bitsPerPixel, compression, sizeImage,
                          xPelsPerM, yPelsPerM, clrUsed, clrImportant}) =
                let
                    (* The passed in value for sizeImage may be wrong.  Call
                       GetDIBits to find the correct value. *)
                    open Memory
                    infix 6 ++
                    local
                        (* This call will build a colour map so we have to have enough
                           space for it. The biggest possible is with 8 bits. *)
                        val w = malloc (rtypeSize + 0w256 * sizeColourEntry)
                        val _ = toR(w, (Word.toInt rtypeSize, width, height, planes, bitsPerPixel,
                                    compression, sizeImage, xPelsPerM, yPelsPerM, clrUsed,
                                    clrImportant))
                        val _ =
                            checkResult(getDIBits(hdc, hb, startScan, scanLines, null, w, DIB_RGB_COLORS) <> 0)
                                handle ex => (free w; raise ex)
                    in
                        val (_, _, _, _, _, _, sizeImage, _, _, _, _) = fromR w
                        val () = free w
                    end
                    
                    (* Calculate the size of the palette. *)
                    val numColours =
                        if clrUsed <> 0
                        then clrUsed
                        else if bitsPerPixel < 16
                        then IntInf.<<(1, Word.fromInt bitsPerPixel)
                        else if compression = BI_BITFIELDS
                        then 3 (* These are DWORD colour masks not RGBQUADS. *)
                        else 0 (* No colour table. *)
                    val bitOffset = rtypeSize + Word.fromInt numColours * sizeColourEntry
                    val size = bitOffset + Word.fromInt sizeImage
                    val w = malloc size
                    val _ = toR(w, (Word.toInt rtypeSize, width, height, planes, bitsPerPixel,
                                compression, sizeImage, xPelsPerM, yPelsPerM, clrUsed,
                                clrImportant))
                    val _ =
                        checkResult(getDIBits(hdc, hb, startScan, scanLines, w ++ bitOffset, w, DIB_RGB_COLORS) <> 0)
                            handle ex => (free w; raise ex)
                in
                    fromCWord8vec (w, Word.toInt size) before free w
                end

        
            fun SetDIBits(hdc, hb, startScan, scanLines, w) =
            let
                open Memory
                infix 6 ++
                val v = toCWord8vec w
                (*val v = toCbytes w*)
                (* We need to work out the offset of the bits.  For this we need
                   the size of the header structure (which may not be a
                   BITMAPINFOHEADER but some other version of it), the number of
                   colours and the compression. *)
                val hdrSize = #1 (fromR v)
                val { clrUsed, compression, bitsPerPixel, ...} = getBitmapInfoHdr w
                val numColours =
                    if clrUsed <> 0
                    then clrUsed
                    else if bitsPerPixel < 16
                    then IntInf.<<(1, Word.fromInt bitsPerPixel)
                    else if compression = BI_BITFIELDS
                    then 3 (* These are DWORD colour masks not RGBQUADS. *)
                    else 0 (* No colour table. *)
                val bitOffset = Word.fromInt hdrSize +Word.fromInt numColours * sizeColourEntry
                val res = setDIBits(hdc, hb, startScan, scanLines,
                        v ++ bitOffset, v, DIB_RGB_COLORS)
            in
                checkResult(res <> 0)
            end
        end

        (* GetBitmapBits and SetBitmapBits are supposedly obsolete but they're useful
           for copying device-dependent bitmaps. *)
        fun GetBitmapBits(hbm, bytes): Word8Vector.vector =
        let
            val gbb = winCall3 (gdi "GetBitmapBits") (cHBITMAP, cDWORD, cPointer) cLong
            open Memory
            val buff = malloc (Word.fromInt bytes)
            val () =
                checkResult(gbb(hbm, bytes, buff) > 0)
                    handle ex => (free buff; raise ex)
        in
            fromCWord8vec (buff, bytes) before free buff
        end

        fun SetBitmapBits(hbm, w) = 
        let
            val sbb = winCall3 (gdi "SetBitmapBits") (cHBITMAP, cDWORD, cPointer) cLong
            val buff = toCWord8vec w
            open Memory
            val () =
                checkResult(sbb(hbm, Word8Vector.length w, buff) > 0)
                    handle ex => (free buff; raise ex)
        in
            free buff
        end

        (*
        Other Bitmap functions:
            AlphaBlend  
            CreateDIBitmap  
            CreateDIBSection  - This creates an area of memory to write to - won't work in ML.
            GetDIBColorTable  
            GradientFill  
            SetDIBColorTable  
            SetDIBitsToDevice  
            SetPixelV  
            StretchDIBits   
            TransparentBlt  
        *)

        end
end;
