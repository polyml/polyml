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
    val GetSystemPaletteEntries : HDC * int * int -> PALETTEENTRY list
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
        open Foreign Base
    in
        type HDC = HDC and HPALETTE = HPALETTE
        open GdiBase


        local
            datatype SystemPaletteUse =
            W of int
        in
            type SystemPaletteUse = SystemPaletteUse
            val SYSTEMPALETTEUSE = absConversion {abs = W, rep = fn W n => n} cUint
        
            val SYSPAL_ERROR                                 = W (0)
            val SYSPAL_STATIC                                = W (1)
            val SYSPAL_NOSTATIC                              = W (2)
        end

        datatype PaletteEntryFlag = PC_NULL | PC_RESERVED | PC_EXPLICIT | PC_NOCOLLAPSE
        type PALETTEENTRY = {red: int, green: int, blue: int, flags: PaletteEntryFlag}

        local
            val cPaletteEnt = cStruct4(cUint8, cUint8, cUint8, cUint8)
            val { load=loadPE, store=storePE, ctype={size=peSize, ...} } = breakConversion cPaletteEnt

            fun toPE({red, green, blue, flags}: PALETTEENTRY) =
            let
                val f =
                    case flags of PC_NULL => 0 | PC_RESERVED => 1
                    | PC_EXPLICIT => 2 | PC_NOCOLLAPSE => 4
            in
                (red, green, blue, f)
            end
            fun fromPE (red, green, blue, f): PALETTEENTRY =
            let
                val flags =
                    case f of
                        0 => PC_NULL
                    |   1 => PC_RESERVED
                    |   2 => PC_EXPLICIT
                    |   4 => PC_NOCOLLAPSE
                    |   _ => raise Match
            in
                {red=red, green=green, blue=blue, flags=flags}
            end
            
            open Memory
            infix 6 ++
            val logPal = cStruct2(cWORD, cWORD)
            val {store=storeLP, ctype={size=lpSize, ...}, ...} = breakConversion logPal
        in
            (* Unfortunately we can't make a simple conversion here.  When we load
               the entries we need to know how many we're loading. *)
            fun allocPEVec n = malloc(Word.fromInt n * peSize)
            val freePEVec = free

            local
                (* Copy the elements into the array. *)
                fun doStore (pe: PALETTEENTRY, vec) =
                (
                    ignore(storePE(vec, toPE pe)); (* Ignore result - nothing to free *)
                    vec ++ peSize
                )
            in
                fun palListToC pl =
                let
                    val count = List.length pl
                    val vec = allocPEVec count
                    val _ = List.foldl doStore vec pl
                in
                    (vec, count)
                end
            
                fun logPaletteToC pl =
                let
                    (* A logical palette has two additional words at the start. *)
                    val count = List.length pl
                    val vec = malloc(Word.fromInt count * peSize + lpSize)
                    val _ = storeLP(vec, (0x300, count))                
                    val _ = List.foldl doStore (vec ++ lpSize) pl
                in
                    vec
                end
            end

            fun palListFromC(vec, count) =
            let
                fun loadPalE n = fromPE(loadPE(vec ++ Word.fromInt n * peSize))
            in
                List.tabulate(count, loadPalE)
            end
        end
        
        val GetSystemPaletteUse        = winCall1(gdi "GetSystemPaletteUse") (cHDC) SYSTEMPALETTEUSE
        val RealizePalette             = winCall1(gdi "RealizePalette") (cHDC) cUint
        val ResizePalette              = winCall2(gdi "ResizePalette") (cHPALETTE,cUint) (successState "ResizePalette")
        val SelectPalette              = winCall3(gdi "SelectPalette") (cHDC,cHPALETTE,cBool) cHPALETTE
        val SetSystemPaletteUse        = winCall2(gdi "SetSystemPaletteUse") (cHDC,SYSTEMPALETTEUSE) SYSTEMPALETTEUSE
        val UpdateColors               = winCall1(gdi "UpdateColors") (cHDC) (successState "UpdateColors")
        val CreateHalftonePalette      = winCall1(gdi "CreateHalftonePalette") (cHDC) cHPALETTE
        val GetNearestColor = winCall2 (gdi "GetNearestColor") (cHDC,cCOLORREF) cCOLORREF 
        val GetNearestPaletteIndex = winCall2 (gdi "GetNearestPaletteIndex") (cHPALETTE,cCOLORREF) cUint
        val UnrealizeObject              = winCall1(gdi "UnrealizeObject") (cHPALETTE) (successState "UnrealizeObject")
        
        local
            val animatePalette =
                winCall4 (gdi "AnimatePalette") (cHPALETTE, cUint, cUint, cPointer) (cBool)
        in
            fun AnimatePalette (h,start,pl) =
            let
                val (vec, count) = palListToC pl
                val res =
                    animatePalette(h, start, count, vec)
                        handle ex => (freePEVec vec; raise ex)
                val () = freePEVec vec
            in 
                res
            end
        end 

        local
            val createPalette = winCall1 (gdi "CreatePalette") (cPointer) (cHPALETTE)
        in
            fun CreatePalette pl =
            let
                val vec = logPaletteToC pl
                val res =
                    createPalette vec handle ex => (freePEVec vec; raise ex)
                val () = freePEVec vec
                val () = checkResult(not(isHNull res))
            in
                res
            end
        end

        local
            val getPaletteEntries =
                winCall4 (gdi "GetPaletteEntries") (cHPALETTE, cUint, cUint, cPointer) cUint
        in
            fun GetPaletteEntries (h, start, no) = 
            let
                val vec = allocPEVec no
                val res = getPaletteEntries (h, start, no, vec)
                (* The result is zero if error *)
                val result = palListFromC(vec, res)
                val () = freePEVec vec
                val () = checkResult(res <> 0)
            in
                result
            end 
        end

        local
            val getSystemPaletteEntries =
                winCall4 (gdi "GetSystemPaletteEntries") (cHDC, cUint, cUint, cPointer) cUint
        in
            fun GetSystemPaletteEntries (h, start, no) = 
            let
                val vec = allocPEVec no
                val res = getSystemPaletteEntries (h, start, no, vec)
                (* The result is zero if error *)
                val result = palListFromC(vec, res)
                val () = freePEVec vec
                val () = checkResult(res <> 0)
            in
                result
            end 
        end

        local
            val setPaletteEntries =
                winCall4 (gdi "SetPaletteEntries") (cHPALETTE, cUint, cUint, cPointer) cUint
        in
            fun SetPaletteEntries (h, start,pl) =
            let 
                val (vec, count) = palListToC pl
                val res =
                    setPaletteEntries(h, start, count, vec)
                        handle ex => (freePEVec vec; raise ex)
                val () = freePEVec vec
            in 
                checkResult(res <> 0)
            end
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
