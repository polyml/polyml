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

structure Color (* Use American spelling for consistency. *):
  sig
    type HPALETTE and HDC

    datatype
      PaletteEntryFlag = PC_EXPLICIT | PC_NOCOLLAPSE | PC_NULL | PC_RESERVED
    type PALETTEENTRY = {red: int, green: int, blue: int, flags: PaletteEntryFlag}

    type COLORREF
    val toRGB :
       COLORREF -> {red: Int.int, blue: Int.int, green: Int.int}
    val RGB : {red: int, blue: int, green: int} -> COLORREF
    val PALETTERGB : {red: int, blue: int, green: int} -> COLORREF

    type SystemPaletteUse
    val SYSPAL_ERROR : SystemPaletteUse
    val SYSPAL_NOSTATIC : SystemPaletteUse
    val SYSPAL_STATIC : SystemPaletteUse

    val AnimatePalette : HPALETTE * int * PALETTEENTRY list -> bool
    val CreateHalftonePalette : HDC -> HPALETTE
    val CreatePalette : PALETTEENTRY list -> HPALETTE
    val GetNearestColor : HDC * COLORREF -> COLORREF
    val GetNearestPaletteIndex : HPALETTE * COLORREF -> int
    val GetPaletteEntries : HPALETTE * int * int -> PALETTEENTRY list
    val GetSystemPaletteEntries : HPALETTE * int * int -> PALETTEENTRY list
    val GetSystemPaletteUse : HDC -> SystemPaletteUse
    val RealizePalette : HDC -> int
    val ResizePalette : HPALETTE * int -> unit
    val SelectPalette : HDC * HPALETTE * bool -> HPALETTE
    val SetPaletteEntries : HPALETTE * int * PALETTEENTRY list -> unit
    val SetSystemPaletteUse : HDC * SystemPaletteUse -> SystemPaletteUse
    val UnrealizeObject : HPALETTE -> unit
    val UpdateColors : HDC -> unit
  end =
struct
    local
        open CInterface Base
    in
        type HDC = HDC and HPALETTE = HPALETTE
        open GdiBase


        local
            datatype SystemPaletteUse =
            W of int
        in
            type SystemPaletteUse = SystemPaletteUse
            val SYSTEMPALETTEUSE = absConversion {abs = W, rep = fn W n => n} INT
        
            val SYSPAL_ERROR                                 = W (0)
            val SYSPAL_STATIC                                = W (1)
            val SYSPAL_NOSTATIC                              = W (2)
        end

        datatype PaletteEntryFlag = PC_NULL | PC_RESERVED | PC_EXPLICIT | PC_NOCOLLAPSE
        type PALETTEENTRY = {red: int, green: int, blue: int, flags: PaletteEntryFlag}

        local
            open LargeWord
            infix 7 andb
            infix 6 orb
            infix 4 << >>

            fun toPE({red, green, blue, flags}: PALETTEENTRY) =
                fromInt red andb 0wxff
                      orb (fromInt green andb 0wxff << 0w8)
                      orb (fromInt blue andb 0wxff << 0w16)
                      orb (case flags of PC_NULL => 0w0 | PC_RESERVED => 0w1 << 0w24
                                | PC_EXPLICIT => 0w2 << 0w24 | PC_NOCOLLAPSE => 0w4 << 0w24)
            fun fromPE w : PALETTEENTRY =
            let
                val red = toInt(w andb 0wxff)
                val green = toInt((w >> 0w8) andb 0wxff)
                val blue = toInt((w >> 0w16) andb 0wxff)
                val flags =
                    case toInt(w >> 0w16) of
                        0 => PC_NULL
                    |   1 => PC_RESERVED
                    |   2 => PC_EXPLICIT
                    |   4 => PC_NOCOLLAPSE
                    |   _ => raise Match
            in
                {red=red, green=green, blue=blue, flags=flags}: PALETTEENTRY
            end
        in
            val PALETTEENTRY = absConversion{abs = fromPE, rep = toPE} WORD
        end

        
        val GetSystemPaletteUse        = call1(gdi "GetSystemPaletteUse") (HDC) SYSTEMPALETTEUSE
        val RealizePalette             = call1(gdi "RealizePalette") (HDC) INT
        val ResizePalette              = call2(gdi "ResizePalette") (HPALETTE,INT) (SUCCESSSTATE "ResizePalette")
        val SelectPalette              = call3(gdi "SelectPalette") (HDC,HPALETTE,BOOL) HPALETTE
        val SetSystemPaletteUse        = call2(gdi "SetSystemPaletteUse") (HDC,SYSTEMPALETTEUSE) SYSTEMPALETTEUSE
        val UpdateColors               = call1(gdi "UpdateColors") (HDC) (SUCCESSSTATE "UpdateColors")
        val CreateHalftonePalette      = call1(gdi "CreateHalftonePalette") (HDC) HPALETTE
        val GetNearestColor = call2 (gdi "GetNearestColor") (HDC,COLORREF) COLORREF 
        val GetNearestPaletteIndex = call2 (gdi "GetNearestPaletteIndex") (HPALETTE,COLORREF) (INT)
        val UnrealizeObject              = call1(gdi "UnrealizeObject") (HPALETTE) (SUCCESSSTATE "UnrealizeObject")
        
        fun AnimatePalette (h,start,pl) =
        let val count = List.length pl
            val (_, toPE, paletteEntry) = breakConversion PALETTEENTRY
            val pal = alloc count paletteEntry
            (* Copy the elements into the array. *)
            val _ = List.foldl (fn (pe, n) => (assign Clong (offset n Clong pal) (toPE pe); n+1)) 0 pl
        
        in call4 (gdi "AnimatePalette")
                 (HPALETTE,INT,INT,POINTER) (BOOL)
                 (h,start,count,address pal)
        end 
        
        fun CreatePalette (pl) =
        let val count = List.length pl
            val (_, toPE, paletteEntry) = breakConversion PALETTEENTRY
            (* A LOGPALETTE consists of a version number, the number of palette entries
               and the entries themselves.  It seems the version is 0x300. *)
            val logpal = alloc (count+1) Clong
            val _ = assign Cshort (offset 0 Cshort logpal) (toCshort 0x300)
            val u = assign Cshort (offset 1 Cshort logpal) (toCshort count)
            val _ = List.foldl (fn (pe, n) => (assign Clong (offset n Clong logpal) (toPE pe); n+1)) 1 pl
        in
           call1 (gdi "CreatePalette") (POINTER) (HPALETTE) (address logpal)
        end 
        
        fun GetPaletteEntries (h,start,no) = 
        let
          val (fromPE, _, paletteEntry) = breakConversion PALETTEENTRY
          val palarr = alloc no paletteEntry
        
          val res = call4 (gdi "GetPaletteEntries") (HPALETTE,INT,INT,POINTER) (INT)
                    (h,start,no,address palarr)
        in
            if res <= 0
            then raiseSysErr()
            else List.tabulate(res, fn i => fromPE (offset i Clong palarr))
        end 
        
        fun GetSystemPaletteEntries (h,start,no) = 
        let
          val (fromPE, _, paletteEntry) = breakConversion PALETTEENTRY
          val palarr = alloc no paletteEntry
        
          val res = call4 (gdi "GetSystemPaletteEntries") (HPALETTE,INT,INT,POINTER) (INT)
                    (h,start,no,address palarr)
        in
            if res <= 0
            then raiseSysErr()
            else List.tabulate(res, fn i => fromPE (offset i Clong palarr))
        end 
         
        fun SetPaletteEntries (h,start,pl) =
        let val count = List.length pl
            val (_, toPE, paletteEntry) = breakConversion PALETTEENTRY
            val pal = alloc count paletteEntry
            val _ = List.foldl (fn (pe, n) => (assign Clong (offset n Clong pal) (toPE pe); n+1)) 0 pl      
        in call4 (gdi "SetPaletteEntries")
                 (HPALETTE,INT,INT,POINTER) (SUCCESSSTATE "SetPaletteEntries")
                 (h,start,count,address pal)
        end
        (*
        Other Colour functions:
            GetColorAdjustment  
            GetSystemPaletteUse  
            SetColorAdjustment  
        *)

    end
end;

(* Install a pretty printer for COLORREF. *)
local
    open Color
    fun printColorRef _ _ x =
    let
        val {red, green, blue} = toRGB x
    in
        PolyML.PrettyString
            (concat["RGB{red=", Int.toString red,
                   ",green=", Int.toString green,
                   ",blue=", Int.toString blue, "}"])
    end
in
    val _ = PolyML.addPrettyPrinter printColorRef
end;
