(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

(***************************************************************************

  Translation of C macros in ML
  -----------------------------

  Macros (all)

  GetBValue       Retrieves blue-intensity value from 32-bit RGB value 
  GetGValue       Retrieves green-intensity value from 32-bit RGB value 
  GetRValue       Retrieves red-intensity value from 32-bit RGB value 
  HIBYTE          Retrieves the high-order byte 
  HIWORD          Retrieves the high-order word 
  LANGIDFROMLCID  Retrieves language ID from locale ID 
  LOBYTE          Retrieves the low-order byte 
  LOWORD          Retrieves the low-order word 
  MAKEINTATOM     Creates an integer atom 
  MAKEINTRESOURCE Converts a resource identifier into a string 
  MAKELANGID      Creates a language ID from primary and sub ID 
  MAKELCID        Creates a locale ID from language ID 
  MAKELONG        LONG value from two WORDs 
  MAKELPARAM      Creates unsigned long-integer message parameter 
  MAKELRESULT     Creates unsigned long-integer message result 
  MAKEPOINTS      Converts a long value into a POINTS structure 
  MAKEWORD        Creates WORD value from two BYTEs 
  MAKEWPARAM      Creates an unsigned long-integer message parameter 

  max             Larger of two values 
  min             Returns the smaller of two values 
  PALETTEINDEX    Returns a logical-palette index specifier 
  PALETTERGB      Red, green, blue values to COLORREF 
  POINTSTOPOINT   Copies POINTS structure to POINT structure 
  POINTTOPOINTS   Converts POINT structure to POINTS structure 
  PRIMARYLANGID   Extracts primary ID from language ID 
  RGB             Red, green, blue values to RGB 

  SUBLANGID       Extracts sublanguage ID from language ID 
  SORTIDFROMLCID  Retrieves sorting ID from locale ID 
  TEXT            Converts text to Unicode when UNICODE is defined 

 **************************************************************************)

functor MacroFct () =
struct

local
  (*open Prelude; *)

  fun intOfHexDigit c =
   let val c = ord c
   in  if ord "0" <= c andalso c <= ord "9" then c - ord "0" else
       if ord "a" <= c andalso c <= ord "f" then c - ord "a" + 10 else
       if ord "A" <= c andalso c <= ord "F" then c - ord "A" + 10 else 
       raise Match
   end

  fun calc [] = 0
    | calc (n::ns) = n + 16*calc ns

  exception HexFailed

  fun split n = (n mod 2=1,n div 2)

  fun extendBoolOp f (0,0) = 0
  |   extendBoolOp f (n,m) = 
  let 
     val (n0,n) = split n
     val (m0,m) = split m  
  in  
     (if f (n0,m0) then 1 else 0) + 2 * extendBoolOp f (n,m)
  end

  fun extend f (n,m) = (extendBoolOp f (n,m))
  (* open Int *)
  val ** = IntInf.pow
  infix **

in
    fun hex str =
      case explode str of
      "0"::"x"::digs => let val increasingDigs = rev (map intOfHexDigit digs)
                  in  calc increasingDigs
                end 
        | _ => raise HexFailed

    val natOr  = extend (fn (a,b) => a orelse b)

    val natAnd = extend (fn (a,b) => a andalso b)

    val natXor = extend (fn (a,b) => (not a andalso b) orelse (a andalso not b))

    fun shiftNatUp   k n = n * (2 ** k)

    fun shiftNatDown k n = n div (2 ** k)

    exception HexORFailed

    fun hexOR [] = raise HexORFailed
    |   hexOR [h] = h
    |   hexOR (h::t) = natOr (h,hexOR t)
     
end


fun GetRValue(rgb)        = natAnd (rgb,hex "0xFF")       

(* ((BYTE)(rgb)) *)


fun GetGValue(rgb)        = natAnd (natAnd(rgb,hex"0xFFFF") div 256,hex "0xFF")   

(* ((BYTE)(((WORD)(rgb)) >> 8)) *)


fun GetBValue(rgb)        = natAnd (rgb div 65536,hex "0xFF") 

(* ((BYTE)((rgb)>>16))  *)


fun HIBYTE(w)             = natAnd ( natAnd (w,hex "0xFFFF")  div  256 , hex "0xFF")

(* ((BYTE)(((WORD)(w) >> 8) & 0xFF)) *)


fun HIWORD(l)             = natAnd ( l div 65536, hex "0xFFFF")

(* ((WORD)(((DWORD)(l) >> 16) & 0xFFFF)) *)

                            
fun LANGIDFROMLCID(lcid)  = natAnd ( lcid , hex "0xFFFF" )

(* ((WORD  )(lcid)) *)


fun LOBYTE(w)             = natAnd (w,hex "0xFF")

(* ((BYTE)(w)) *)


fun LOWORD(l)             = natAnd (l,hex "0xFFFF")

(* ((WORD)(l)) *)


(* To be used in RGB *)

fun LONG2BYTE dw = 
let
  val wlow  = LOWORD dw
  val b1    = LOBYTE wlow
  val b2    = HIBYTE wlow
  val whigh = HIWORD dw
  val b3    = LOBYTE whigh
  val b4    = HIBYTE whigh
in 
   (b1,b2,b3,b4)
end

fun BYTE2LONG (b1,b2,b3,b4) = 
let
  val b1ok = natAnd (b1,hex "0xFF") 
  val b2ok = natAnd (b2,hex "0xFF")
  val b3ok = natAnd (b3,hex "0xFF")
  val b4ok = natAnd (b4,hex "0xFF")
in
  b1ok + 256 * ( b2ok + 256 * ( b3ok + 256 * b4ok) ) 
end


fun MAKEINTATOM(i)        = natAnd (i,hex "0xFFFF")

(* (LPTSTR)((DWORD)((WORD)(i))) *)


fun MAKEINTRESOURCEA(i)   = natAnd (i,hex "0xFFFF")

(* (LPSTR)((DWORD)((WORD)(i))) *)


fun MAKELANGID(p, s)      = natOr ( natAnd(s,hex"0xFFFF")*1024, 
                                    natAnd(p,hex"0xFFFF")     )

(* ((((WORD)(s)) << 10) | (WORD)(p)) *)


fun MAKELCID(lgid, srtid) = natOr( natAnd(srtid,hex"0xFFFF")*65536, 
                                   natAnd(lgid,hex"0xFFFF")  )

(* ((DWORD)( ( ( (DWORD) ((WORD)(srtid)) ) << 16) | ( (DWORD) ((WORD)(lgid)) ) )) *)


fun MAKELONG(a, b)        = natOr ( natAnd(a,hex"0xFFFF"),
                                    natAnd(b,hex"0xFFFF") * 65536 )       

(* ((LONG)(((WORD)(a)) | ((DWORD)((WORD)(b))) << 16)) *)


fun MAKELPARAM(l, h)      = MAKELONG(l, h)

(* (LPARAM)MAKELONG(l, h) *)


fun MAKELRESULT(l, h)     = MAKELONG(l, h)

(* (LRESULT)MAKELONG(l, h) *)


fun MAKEPOINTS (l)         = 0 (* Not implemented *)

(*   (*((POINTS FAR *)&(l)))   *)


fun MAKEWORD(a, b)        = natAnd ( natOr( natAnd(a,hex"0xFF"),
                                     natAnd(natAnd(b,hex"0xFF")*256,hex"0xFFFF") ),
                                     hex"0xFFFF" )

(* ((WORD)(((BYTE)(a)) | ((WORD)((BYTE)(b))) << 8)) *)


fun MAKEWPARAM(l, h)      = MAKELONG(l, h)

(* (WPARAM)MAKELONG(l, h) *)


fun MAX(a:int,b:int)              = if a > b then a else b


fun MIN(a:int,b:int)              = if a < b then a else b


fun PALETTEINDEX(i)       =  natOr(hex"0x01000000",
                                   natAnd(i,hex"FFFF") )  

(* ((COLORREF)(0x01000000 | (DWORD)(WORD)(i))) *)


fun POINTSTOPOINT(pt,pts) = 0 (* Not implemented *)

(* {(pt).x = (SHORT)LOWORD(pts); (pt).y = (SHORT)HIWORD(pts);} *)


fun POINTTOPOINTS(pt)     = 0 (* Not implemented *) 

(* (MAKELONG((short)((pt).x), (short)((pt).y))) *)


fun PRIMARYLANGID(lgid)   = natAnd ( natAnd(lgid,hex"0xFFFF"), hex"0x3FF") 

(* ((WORD)(lgid) & 0x3ff) *)


fun RGB(r,g,b)            = hexOR ([natAnd(r,hex"0xFF"),
                                            natAnd(natAnd(g,hex"0xFF")*256,hex"0xFFFF"),
                                            natAnd(natAnd(b,hex"0xFF")*65536,hex"0xFFFFFFFF")] 
                                  )

(* ((COLORREF)  (((BYTE)(r) | ((WORD)((BYTE)(g))<<8)) | (((DWORD)(BYTE)(b))<<16))) *)


fun PALETTERGB(r,g,b)     = natOr( hex"0x02000000", RGB(r,g,b) ) 

(* (0x02000000 | RGB(r,g,b)) *)


fun SUBLANGID(lgid)       = natAnd(lgid,hex"0xFFFF")  div  1024

(* ((WORD  )(lgid) >> 10) *) 


val NLS_VALID_LOCALE_MASK = hex ("0x000FFFFF")


fun SORTIDFROMLCID(lcid)  =  natAnd( natAnd (lcid,
                                             NLS_VALID_LOCALE_MASK )  div  65536,
                                     hex"0xFFFF" )

(* ((WORD  )((((DWORD)(lcid)) & NLS_VALID_LOCALE_MASK) >> 16)) *)


fun TEXT (s) = s


end;









