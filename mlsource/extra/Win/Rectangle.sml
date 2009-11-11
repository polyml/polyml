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

structure Rectangle :
  sig
    type POINT = { x: int, y: int }
    type RECT = { top: int, left: int, bottom: int, right: int }

    val EqualRect : RECT * RECT -> bool
    val InflateRect : RECT * int * int -> unit * RECT
    val IntersectRect : RECT * RECT -> bool * RECT
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
        open CInterface Base
        fun calluser name = call_sym (load_sym (load_lib "user32.DLL") name)

        fun usercall_MII name CR (C1,C2,C3) (a1,a2,a3) =
            let val (from1,to1,ctype1) = breakConversion C1
                val (from2,to2,ctype2) = breakConversion C2
                val (from3,to3,ctype3) = breakConversion C3
                val (fromR,toR,ctypeR) = breakConversion CR
                val va1 = address (to1 a1)
                val va2 = to2 a2
                val va3 = to3 a3
                val res = calluser name [(Cpointer ctype1,va1),(ctype2,va2),(ctype3,va3)] ctypeR
            in  (fromR res,from1 (deref va1))
            end
    in
        type RECT = RECT and POINT = POINT

        (* RECTANGLES. *)
        val EqualRect              = call2 (user "EqualRect") (POINTERTO RECT, POINTERTO RECT) BOOL
        val InflateRect            = usercall_MII "InflateRect" (SUCCESSSTATE "InflateRect") (RECT,INT,INT)

        fun IntersectRect(r1, r2) =
        let
            val (from,_,rtype) = breakConversion RECT 
            val va = address (alloc 1 rtype)
            val res = call3 (user "IntersectRect") (POINTER, POINTERTO RECT, POINTERTO RECT) BOOL
                        (va, r1, r2)
        in
            (res, from(deref va))
        end

        fun OffsetRect(r: RECT, dx: int, dy: int): RECT =
        let
            val (from,_,rtype) = breakConversion RECT 
            val va = address (alloc 1 rtype)
        in
            call3 (user "OffsetRect") (POINTER, INT, INT) (SUCCESSSTATE "OffsetRect")(va, dx, dy);
            from(deref va)
        end

        val IsRectEmpty            = call1(user "IsRectEmpty") (POINTERTO RECT) BOOL
        val PtInRect              = call2(user "PtInRect") (POINTERTO RECT,POINT) BOOL

        
        fun SetRect(a,b,c,d) : RECT =
        let
            val (from,_,rtype) = breakConversion RECT 
            val va = address (alloc 1 rtype)
        in
            call5 (user "SetRect") (POINTER, INT, INT, INT, INT) (SUCCESSSTATE "SetRect")
                (va, a, b, c, d);
            from(deref va)
        end

        fun SetRectEmpty () : RECT =
        let
            val (from,_,rtype) = breakConversion RECT 
            val va = address (alloc 1 rtype)
        in
            call1 (user "SetRectEmpty") (POINTER) (SUCCESSSTATE "SetRectEmpty") va;
            from(deref va)
        end

        local
            fun rectCall name (r1: RECT, r2: RECT) : RECT =
            let
                val (from,_,rtype) = breakConversion RECT 
                val va = address (alloc 1 rtype)
            in
                call3 (user name) (POINTER, POINTERTO RECT, POINTERTO RECT) (SUCCESSSTATE name)
                    (va, r1, r2);
                from(deref va)
            end
        in
            val SubtractRect = rectCall "SubtractRect"
            val UnionRect    = rectCall "UnionRect"
        end

        (*
            Other Rectangle functions:
                CopyRect  
        *)
    end
end;
