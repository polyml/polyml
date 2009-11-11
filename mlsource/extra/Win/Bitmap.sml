(*
    Copyright (c) 2001-7
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
        open CInterface Base

        fun callgdi name = call_sym (load_sym (load_lib "gdi32.DLL") name)

        fun gdicall_IIIM name CR (C1,C2,C3,C4) (a1,a2,a3,a4) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (from4,to4,ctype4) = breakConversion C4
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = to1 a1
                val va2 = to2 a2
                val va3 = to3 a3
                val va4 = address (to4 a4)
                val res = callgdi name [(ctype1,va1),(ctype2,va2),(ctype3,va3),(Cpointer ctype4,va4)] ctypeR
                val _ : unit = fromR res
            in  from4 (deref va4)
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

        val XCOORD = INT : int Conversion
        val YCOORD = INT: int Conversion
        val XOFFSET = INT: int Conversion
        val YOFFSET = INT: int Conversion
        val WIDTH = INT: int Conversion
        val HEIGHT = INT: int Conversion

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
            val STRETCHMODE = absConversion {abs = W, rep = fn W n => n} INT
        
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
            val FLOODFILLMODE = tableConversion(tab, NONE)
        end

        val ExtFloodFill =
            call5 (gdi "ExtFloodFill") 
                   (HDC,XCOORD,YCOORD,COLORREF,FLOODFILLMODE) (SUCCESSSTATE "ExtFloodFill")

        val GetPixel = call3 (gdi "GetPixel") (HDC,INT,INT) COLORREF
        val SetPixel = call4 (gdi "SetPixel") (HDC,INT,INT, COLORREF) COLORREF
        val BitBlt = call9 (gdi  "BitBlt") (HDC,XCOORD,YCOORD,WIDTH,HEIGHT,HDC,XCOORD,YCOORD,RASTEROPCODE)
                (SUCCESSSTATE "BitBlt")
                                         

        val CreateCompatibleBitmap     = 
            checkBitmap o
                call3 (gdi "CreateCompatibleBitmap") (HDC,WIDTH,HEIGHT) HBITMAP


        val GetStretchBltMode          = call1 (gdi "GetStretchBltMode") (HDC) STRETCHMODE

        (* TODO: The raster op is supposed to be a combined operation for the foreground and
           background. *)
        val MaskBlt                    = call12(gdi "MaskBlt")
                                         (HDC,XCOORD,YCOORD,WIDTH,HEIGHT,HDC,XCOORD,YCOORD,HBITMAP,XOFFSET,
                                          YOFFSET,QUATERNARY)
                                          (SUCCESSSTATE "MaskBlt")

        val SetStretchBltMode          = call2(gdi "SetStretchBltMode") (HDC,STRETCHMODE) (SUCCESSSTATE "SetStretchBltMode")

        val StretchBlt =
            call11(gdi "StretchBlt") 
                (HDC,XCOORD,YCOORD,WIDTH,HEIGHT,HDC,XCOORD,YCOORD,WIDTH,HEIGHT,RASTEROPCODE)
                    (SUCCESSSTATE "StretchBlt")

        (* This definitely has the wrong type. *)
        (*val PlgBlt = call7 (gdi "PlgBlt")(HDC,RECT,HDC,RECT,HBITMAP,XCOORD,YCOORD)
                 (SUCCESSSTATE "PlgBlt")*)
                                         

        val SetBitmapDimensionEx       = gdicall_IIIM "SetBitmapDimensionEx" (SUCCESSSTATE "SetBitmapDimensionEx")
                                         (HBITMAP,WIDTH,HEIGHT,SIZE)

        val GetBitmapDimensionEx       = gdicall_IW "GetBitmapDimensionEx" (SUCCESSSTATE "GetBitmapDimensionEx")
                                         (HBITMAP,SIZE)

        val CreateBitmapIndirect       =
            checkBitmap o
                call1 (gdi "CreateBitmapIndirect") (POINTERTO BITMAP) HBITMAP

        local
            val cbm = checkBitmap o
                call5 (gdi "CreateBitmap") (INT, INT, INT, INT, POINTER) HBITMAP
        in
            fun CreateBitmap{width, height, planes, bitsPerPixel, bits} =
                cbm(width, height, planes, bitsPerPixel,
                    case bits of NONE => toCint 0
                    |   SOME v => fromWord8vec v)
        end

        local
            (* RGBQUAD values are four bytes of blue, green, red and a reserved byte. *)
            val RGBQUAD = STRUCT4(CHAR, CHAR, CHAR, CHAR)
            val (fromR, toR, rtype) = breakConversion RGBQUAD
            fun from v =
                let val (b, g, r, _) = fromR v in {red = ord r, blue = ord b, green = ord g} end
            fun to {red, green, blue} = toR(chr blue, chr green, chr red, #"\000")
        in
            val RGBQUAD = mkConversion from to rtype
        end

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
            val BITCOMPRESSION = absConversion {abs = toComp, rep = fromComp} INT
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
            val DIB_PAL_COLORS =     1

            val BITMAPINFOHEADER = STRUCT11(INT, LONG, LONG, SHORT, SHORT, BITCOMPRESSION,
                INT, LONG, LONG, INT, INT)
            val (fromR, toR, rtype) = breakConversion BITMAPINFOHEADER

            val getDIBits = call7 (gdi "GetDIBits")
                (HDC, HBITMAP, UINT, UINT, POINTER, POINTER, INT) INT

            val setDIBits = call7 (gdi "SetDIBits")
                (HDC, HBITMAP, UINT, UINT, POINTER, POINTER, INT) INT
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
                    val v = toR(sizeof rtype, 0, 0, 0, 0, BI_RGB, 0, 0, 0, 0, 0)
                    val res = getDIBits(hdc, hb, startScan, scanLines, toCint 0,
                                    address v, DIB_RGB_COLORS)
                in
                    checkResult(res <> 0);
                    fromCbytes(address v, sizeof rtype)
                end

             |  GetDIBits(hdc: HDC, hb: HBITMAP, startScan, scanLines,
                    SOME {width, height, planes, bitsPerPixel, compression, sizeImage,
                          xPelsPerM, yPelsPerM, clrUsed, clrImportant}) =
                let
                    (* The passed in value for sizeImage may be wrong.  Call
                       GetDIBits to find the correct value. *)
                    val v = toR(sizeof rtype, width, height, planes, bitsPerPixel,
                                compression, sizeImage, xPelsPerM, yPelsPerM, clrUsed,
                                clrImportant)
                    (* This call will build a colour map so we have to have enough
                       space for it. The biggest possible is with 8 bits. *)
                    val w = alloc (sizeof rtype + 256*sizeof Cint) Cchar
                    val _ = assign rtype w v
                    val res = getDIBits(hdc, hb, startScan, scanLines, toCint 0,
                                    address w, DIB_RGB_COLORS)
                    val _ = checkResult(res <> 0)
                    val (_, _, _, _, _, _, sizeImage, _, _, _, _) = fromR w
                    (* Calculate the size of the palette. *)
                    val numColours =
                        if clrUsed <> 0
                        then clrUsed
                        else if bitsPerPixel < 16
                        then IntInf.<<(1, Word.fromInt bitsPerPixel)
                        else if compression = BI_BITFIELDS
                        then 3 (* These are DWORD colour masks not RGBQUADS. *)
                        else 0 (* No colour table. *)
                    val bitOffset = sizeof rtype + numColours * sizeof Cint
                    val size = bitOffset + sizeImage
                    val mem = alloc size Cchar
                    val _ = assign rtype mem v
                    val res = getDIBits(hdc, hb, startScan, scanLines,
                                address(offset bitOffset Cchar mem),
                                address mem, DIB_RGB_COLORS)
                in
                    checkResult(res <> 0);
                    fromCbytes(address mem, size)                   
                end

        
            fun SetDIBits(hdc, hb, startScan, scanLines, w) =
            let
                val v = toCbytes w
                (* We need to work out the offset of the bits.  For this we need
                   the size of the header structure (which may not be a
                   BITMAPINFOHEADER but some other version of it), the number of
                   colours and the compression. *)
                val hdrSize = LargeWord.toInt(PackWord32Little.subVec(w, 0))
                val { clrUsed, compression, bitsPerPixel, ...} = getBitmapInfoHdr w
                val numColours =
                    if clrUsed <> 0
                    then clrUsed
                    else if bitsPerPixel < 16
                    then IntInf.<<(1, Word.fromInt bitsPerPixel)
                    else if compression = BI_BITFIELDS
                    then 3 (* These are DWORD colour masks not RGBQUADS. *)
                    else 0 (* No colour table. *)
                val bitOffset = hdrSize + numColours * sizeof Cint
                val res = setDIBits(hdc, hb, startScan, scanLines,
                        address(offset bitOffset Cchar (deref v)), v, DIB_RGB_COLORS)
            in
                checkResult(res <> 0)
            end
        end

        (* GetBitmapBits and SetBitmapBits are supposedly obsolete but they're useful
           for copying device-dependent bitmaps. *)
        fun GetBitmapBits(hbm, bytes): Word8Vector.vector =
        let
            val gbb = call3 (gdi "GetBitmapBits") (HBITMAP, INT, POINTER) (POSINT "GetBitmapBits")
            val buff = alloc bytes Cchar
            val res = gbb(hbm, bytes, address buff)
        in
            toWord8vec (address buff, bytes)
        end

        fun SetBitmapBits(hbm, w) = 
        let
            val sbb = call3 (gdi "SetBitmapBits") (HBITMAP, INT, POINTER) (POSINT "SetBitmapBits")
            val buff = fromWord8vec w
            val res = sbb(hbm, Word8Vector.length w, buff)
        in
            ()
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
