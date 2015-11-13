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

structure Rectangle :
  sig
    type POINT = { x: int, y: int }
    type RECT = { top: int, left: int, bottom: int, right: int }

    val EqualRect : RECT * RECT -> bool
    val InflateRect : RECT * int * int -> RECT
    val IntersectRect : RECT * RECT -> RECT option
    val IsRectEmpty : RECT -> bool
    val OffsetRect : RECT * int * int -> RECT
    val PtInRect : RECT * POINT -> bool
    val SetRect : int * int * int * int -> RECT
    val SetRectEmpty : unit -> RECT
    val SubtractRect : RECT * RECT -> RECT
    val UnionRect : RECT * RECT -> RECT
  end =
struct
    local
        open Foreign Base
(*        fun usercall_MII name CR (C1,C2,C3) (a1,a2,a3) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = address (to1 a1)
                val va2 = to2 a2
                val va3 = to3 a3
                val res = calluser name [(Cpointer ctype1,va1),(ctype2,va2),(ctype3,va3)] ctypeR
            in  (fromR res,from1 (deref va1))
            end*)
    in
        type RECT = RECT and POINT = POINT
        (* TODO: It would be a lot more efficient to implement these directly in ML. *)
        
        val zeroRect: RECT = {top=0, bottom=0, left=0, right=0}

        (* RECTANGLES. *)
        val EqualRect = call2 (user "EqualRect") (cConstStar cRect, cConstStar cRect) cBool
        
        local
            val inflateRect = call3 (user "InflateRect")  (cStar cRect, cInt, cInt) (successState "InflateRect")
        in
            fun InflateRect(r, x, y) = let val v = ref r in inflateRect(v, x, y); !v end
        end

        local
            val intersectRect = call3 (user "IntersectRect") (cStar cRect, cConstStar cRect, cConstStar cRect) cBool
        in
            fun IntersectRect(r1, r2) =
                let val r = ref zeroRect in if intersectRect(r, r1, r2) then SOME(!r) else NONE end
        end

        local
            val offsetRect = call3 (user "OffsetRect") (cStar cRect, cInt, cInt) (successState "OffsetRect")
        in
            fun OffsetRect(r, x, y) = let val v = ref r in offsetRect(v, x, y); !v end
        end

        val IsRectEmpty = call1(user "IsRectEmpty") (cConstStar cRect) cBool
        val PtInRect = call2(user "PtInRect") (cConstStar cRect, cPoint) cBool

        local
            val setRect = call5 (user "SetRect") (cStar cRect, cInt, cInt, cInt, cInt) (successState "SetRect")
        in
            fun SetRect(a,b,c,d) : RECT = let val v = ref zeroRect in setRect(v, a,b,c,d); !v end
        end
        
        fun SetRectEmpty () : RECT = zeroRect (* No need to call C to do this *)

        local
            val subtractRect =
                call3 (user "SubtractRect") (cStar cRect, cConstStar cRect, cConstStar cRect) (successState "SubtractRect")
            and unionRect =
                call3 (user "UnionRect") (cStar cRect, cConstStar cRect, cConstStar cRect) (successState "UnionRect")
        in
            fun SubtractRect(r1, r2) = let val v = ref zeroRect in subtractRect(v, r1, r2); !v end
            and UnionRect(r1, r2) = let val v = ref zeroRect in unionRect(v, r1, r2); !v end
        end

        (*
            Other Rectangle functions:
                CopyRect  
        *)
    end
end;
