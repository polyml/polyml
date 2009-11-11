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

structure Pen :
  sig
    type HPEN 
    datatype
      PenStyle =
          PS_ALTERNATE
        | PS_COSMETIC
        | PS_DASH
        | PS_DASHDOT
        | PS_DASHDOTDOT
        | PS_DOT
        | PS_ENDCAP_FLAT
        | PS_ENDCAP_ROUND
        | PS_ENDCAP_SQUARE
        | PS_GEOMETRIC
        | PS_INSIDEFRAME
        | PS_JOIN_BEVEL
        | PS_JOIN_MITER
        | PS_JOIN_ROUND
        | PS_NULL
        | PS_SOLID
        | PS_USERSTYLE

    type COLORREF = Color.COLORREF
    type LOGBRUSH = Brush.LOGBRUSH
    type LOGPEN = PenStyle * int option * COLORREF

    val CreatePen : PenStyle list * int * COLORREF -> HPEN
    val CreatePenIndirect : LOGPEN -> HPEN
    val ExtCreatePen : PenStyle list * int * LOGBRUSH * (int * int) list -> HPEN

  end =
struct
    local
        open CInterface Base
    in
        open GdiBase
        type HPEN = HPEN

        val CreatePen = call3 (gdi "CreatePen") (PENSTYLE,INT,COLORREF) (HPEN)
        val CreatePenIndirect = call1 (gdi "CreatePenIndirect") (POINTERTO LOGPEN) (HPEN)

        fun ExtCreatePen (ps: PenStyle list, width, log: LOGBRUSH, dashSp: (int*int) list) = 
        let
            val PAIR = absConversion {abs = fn _ => raise Fail "PAIR", rep = MAKELONG} LONG
            (* custom is supposed to be NULL if ps <> PS_USERSTYLE.  Make sure it is at least
               NULL if the list is empty. *)
            val (custom, len) =
                case dashSp of
                    [] => (toCint 0, 0)
                |   _ => list2Vector PAIR dashSp
        in
           call5 (gdi "ExtCreatePen")
                 (PENSTYLE,LONG,LOGBRUSH,LONG,POINTER) (HPEN)
                 (ps, width, log, len, custom)
        end
        
    end
end;
