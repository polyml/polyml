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


(**********************************************************************
 *
 *  Functor Application
 *
 **********************************************************************)

functor STRUCT_CONVERSIONALS (
    structure CallWithConv  : CallWithConvSig
    structure Struct        : StructSig
    structure ForeignException : ForeignExceptionSig
    structure LowerLevel : LowerLevelSig
    sharing type CallWithConv.vol = Struct.vol = LowerLevel.vol
    sharing Struct.Ctype = CallWithConv.Ctype = LowerLevel.Ctype

) : StructConversionalsSig =
struct    

open Struct;
open CallWithConv;
open Ctype;
open ForeignException;
open LowerLevel;
       
exception Zip;
fun zip [] [] = []
  | zip (x::xs) (y::ys) = (x,y) :: zip xs ys
  | zip _ _ = raise Zip;


exception Break
fun break2 ts v = case break_struct ts v of [a,b]               => (a,b)                | _ => raise Break
fun break3 ts v = case break_struct ts v of [a,b,c]             => (a,b,c)              | _ => raise Break
fun break4 ts v = case break_struct ts v of [a,b,c,d]           => (a,b,c,d)            | _ => raise Break
fun break5 ts v = case break_struct ts v of [a,b,c,d,e]         => (a,b,c,d,e)          | _ => raise Break
fun break6 ts v = case break_struct ts v of [a,b,c,d,e,f]   => (a,b,c,d,e,f)    | _ => raise Break
fun break7 ts v = case break_struct ts v of [a,b,c,d,e,f,g] => (a,b,c,d,e,f,g)  | _ => raise Break
fun break8 ts v = case break_struct ts v of [a,b,c,d,e,f,g,h]   => (a,b,c,d,e,f,g,h)    | _ => raise Break
fun break9 ts v = case break_struct ts v of [a,b,c,d,e,f,g,h,i] => (a,b,c,d,e,f,g,h,i)  | _ => raise Break


fun STRUCT2 (c1,c2) =
    let
    val (from1,to1,ctype1) = breakConversion c1
    val (from2,to2,ctype2) = breakConversion c2

    val ctypes = [ctype1,ctype2]
        
    fun from v =
        let val (v1,v2) = break2 ctypes v
        in (from1 v1,from2 v2)
        end

    fun to (a,b) = make_struct (zip ctypes [to1 a,to2 b])
    in
    mkConversion from to (Cstruct ctypes)
    end
        

fun STRUCT3 (c1,c2,c3) =
    let
    val (from1,to1,ctype1) = breakConversion c1
    val (from2,to2,ctype2) = breakConversion c2
    val (from3,to3,ctype3) = breakConversion c3

    val ctypes = [ctype1,ctype2,ctype3]
        
    fun from v =
        let val (v1,v2,v3) = break3 ctypes v
        in (from1 v1,from2 v2,from3 v3)
        end

    fun to (a,b,c) = make_struct (zip ctypes [to1 a,to2 b,to3 c])
    in
    mkConversion from to (Cstruct ctypes)
    end
        

fun STRUCT4 (c1,c2,c3,c4) =
    let
    val (from1,to1,ctype1) = breakConversion c1
    val (from2,to2,ctype2) = breakConversion c2
    val (from3,to3,ctype3) = breakConversion c3
    val (from4,to4,ctype4) = breakConversion c4

    val ctypes = [ctype1,ctype2,ctype3,ctype4]
        
    fun from v =
        let val (v1,v2,v3,v4) = break4 ctypes v
        in (from1 v1,from2 v2,from3 v3,from4 v4)
        end

    fun to (a,b,c,d) = make_struct (zip ctypes [to1 a,to2 b,to3 c,to4 d])
    in
    mkConversion from to (Cstruct ctypes)
    end


fun STRUCT5 (c1,c2,c3,c4,c5) =
    let
    val (from1,to1,ctype1) = breakConversion c1
    val (from2,to2,ctype2) = breakConversion c2
    val (from3,to3,ctype3) = breakConversion c3
    val (from4,to4,ctype4) = breakConversion c4
    val (from5,to5,ctype5) = breakConversion c5

    val ctypes = [ctype1,ctype2,ctype3,ctype4,ctype5]
        
    fun from v =
        let val (v1,v2,v3,v4,v5) = break5 ctypes v
        in (from1 v1,from2 v2,from3 v3,from4 v4,from5 v5)
        end

    fun to (a,b,c,d,e) = make_struct (zip ctypes [to1 a,to2 b,to3 c,to4 d,to5 e])
    in
    mkConversion from to (Cstruct ctypes)
    end


fun STRUCT6 (c1,c2,c3,c4,c5,c6) =
    let
    val (from1,to1,ctype1) = breakConversion c1
    val (from2,to2,ctype2) = breakConversion c2
    val (from3,to3,ctype3) = breakConversion c3
    val (from4,to4,ctype4) = breakConversion c4
    val (from5,to5,ctype5) = breakConversion c5
    val (from6,to6,ctype6) = breakConversion c6

    val ctypes = [ctype1,ctype2,ctype3,ctype4,ctype5,ctype6]
        
    fun from v =
        let val (v1,v2,v3,v4,v5,v6) = break6 ctypes v
        in (from1 v1,from2 v2,from3 v3,from4 v4,from5 v5,from6 v6)
        end

    fun to (a,b,c,d,e,f) = make_struct (zip ctypes [to1 a,to2 b,to3 c,to4 d,to5 e,to6 f])
    in
    mkConversion from to (Cstruct ctypes)
    end

  
fun STRUCT7 (c1,c2,c3,c4,c5,c6,c7) =
    let
    val (from1,to1,ctype1) = breakConversion c1
    val (from2,to2,ctype2) = breakConversion c2
    val (from3,to3,ctype3) = breakConversion c3
    val (from4,to4,ctype4) = breakConversion c4
    val (from5,to5,ctype5) = breakConversion c5
    val (from6,to6,ctype6) = breakConversion c6
    val (from7,to7,ctype7) = breakConversion c7

    val ctypes = [ctype1,ctype2,ctype3,ctype4,ctype5,ctype6,ctype7]
        
    fun from v =
        let val (v1,v2,v3,v4,v5,v6,v7) = break7 ctypes v
        in (from1 v1,from2 v2,from3 v3,from4 v4,from5 v5,from6 v6,from7 v7)
        end

    fun to (a,b,c,d,e,f,g) = make_struct (zip ctypes [to1 a,to2 b,to3 c,to4 d,to5 e,to6 f,to7 g])
    in
    mkConversion from to (Cstruct ctypes)
    end


fun STRUCT8 (c1,c2,c3,c4,c5,c6,c7,c8) =
    let
    val (from1,to1,ctype1) = breakConversion c1
    val (from2,to2,ctype2) = breakConversion c2
    val (from3,to3,ctype3) = breakConversion c3
    val (from4,to4,ctype4) = breakConversion c4
    val (from5,to5,ctype5) = breakConversion c5
    val (from6,to6,ctype6) = breakConversion c6
    val (from7,to7,ctype7) = breakConversion c7
    val (from8,to8,ctype8) = breakConversion c8

    val ctypes = [ctype1,ctype2,ctype3,ctype4,ctype5,ctype6,ctype7,ctype8]
        
    fun from v =
        let val (v1,v2,v3,v4,v5,v6,v7,v8) = break8 ctypes v
        in (from1 v1,from2 v2,from3 v3,from4 v4,from5 v5,from6 v6,from7 v7,from8 v8)
        end

    fun to (a,b,c,d,e,f,g,h) = make_struct (zip ctypes [to1 a,to2 b,to3 c,to4 d,to5 e,to6 f,to7 g,to8 h])
    in
    mkConversion from to (Cstruct ctypes)
    end


fun STRUCT9 (c1,c2,c3,c4,c5,c6,c7,c8,c9) =
    let
    val (from1,to1,ctype1) = breakConversion c1
    val (from2,to2,ctype2) = breakConversion c2
    val (from3,to3,ctype3) = breakConversion c3
    val (from4,to4,ctype4) = breakConversion c4
    val (from5,to5,ctype5) = breakConversion c5
    val (from6,to6,ctype6) = breakConversion c6
    val (from7,to7,ctype7) = breakConversion c7
    val (from8,to8,ctype8) = breakConversion c8
    val (from9,to9,ctype9) = breakConversion c9

    val ctypes = [ctype1,ctype2,ctype3,ctype4,ctype5,ctype6,ctype7,ctype8,ctype9]
        
    fun from v =
        let val (v1,v2,v3,v4,v5,v6,v7,v8,v9) = break9 ctypes v
        in (from1 v1,from2 v2,from3 v3,from4 v4,from5 v5,from6 v6,from7 v7,from8 v8,from9 v9)
        end

    fun to (a,b,c,d,e,f,g,h,i) = make_struct (zip ctypes [to1 a,to2 b,to3 c,to4 d,to5 e,to6 f,to7 g,to8 h,to9 i])
    in
    mkConversion from to (Cstruct ctypes)
    end

fun FUNCTION0 () (cr: 'a Conversion) =
    let
    val (_, tor, ctyper) = breakConversion cr (* Convert the result type. *)
    fun from _ = raise Foreign "Conversion from C function to ML function is not implemented"
    (* Construct a wrapper function.  The argument is ignored and the result is converted into a vol.*)
    fun convF f _ = tor(f())
    fun to (f: unit->'a) : vol = toCfunction [] ctyper (convF f)
    in
    mkConversion from to (Cfunction ([], ctyper))
    end;

fun PASCALFUNCTION0 () cr =
    let
    val (_, tor, ctyper) = breakConversion cr (* Convert the result type. *)
    fun from _ = raise Foreign "Conversion from C function to ML function is not implemented"
    fun convF f _ = tor(f())
    fun to f = toPascalfunction [] ctyper (convF f)
    in
    mkConversion from to (Cfunction ([], ctyper))
    end;

fun FUNCTION1 c1 cr =
    let
    val (from1,_,ctype1) = breakConversion c1
    val (_,tor,ctyper) = breakConversion cr
    fun from _ = raise Foreign "Conversion from C function to ML function is not implemented"
    (* The wrapper function.  We need a single argument. *)
    fun convF f [a] = tor(f(from1 a))
      | convF _ _ = raise Union.Never "arg mismatch"
    fun to f : vol = toCfunction [ctype1] ctyper (convF f)
    in
    mkConversion from to (Cfunction ([ctype1], ctyper))
    end;

fun PASCALFUNCTION1 c1 cr =
    let
    val (from1,_,ctype1) = breakConversion c1
    val (_,tor,ctyper) = breakConversion cr
    fun from _ = raise Foreign "Conversion from C function to ML function is not implemented"
    fun convF f [a] = tor(f(from1 a))
      | convF _ _ = raise Union.Never "arg mismatch"
    fun to f : vol = toPascalfunction [ctype1] ctyper (convF f)
    in
    mkConversion from to (Cfunction ([ctype1], ctyper))
    end;

fun FUNCTION2 (c1, c2) cr =
    let
    val (from1,_,ctype1) = breakConversion c1
    val (from2,_,ctype2) = breakConversion c2
    val (_,tor,ctyper) = breakConversion cr
    fun from _ = raise Foreign "Conversion from C function to ML function is not implemented"
    fun convF f [a,b] = tor(f(from1 a, from2 b))
      | convF _ _  = raise Union.Never "arg mismatch"
    fun to f = toCfunction [ctype1, ctype2] ctyper (convF f)
    in
    mkConversion from to (Cfunction ([ctype1, ctype2], ctyper))
    end;

fun PASCALFUNCTION2 (c1, c2) cr =
    let
    val (from1,_,ctype1) = breakConversion c1
    val (from2,_,ctype2) = breakConversion c2
    val (_,tor,ctyper) = breakConversion cr
    fun from _ = raise Foreign "Conversion from C function to ML function is not implemented"
    fun convF f [a,b] = tor(f(from1 a, from2 b))
      | convF _ _ = raise Union.Never "arg mismatch"
    fun to f = toPascalfunction [ctype1, ctype2] ctyper (convF f)
    in
    mkConversion from to (Cfunction ([ctype1, ctype2], ctyper))
    end;

fun FUNCTION3 (c1, c2, c3) cr =
    let
    val (from1,_,ctype1) = breakConversion c1
    val (from2,_,ctype2) = breakConversion c2
    val (from3,_,ctype3) = breakConversion c3
    val (_,tor,ctyper) = breakConversion cr
    fun from _ = raise Foreign "Conversion from C function to ML function is not implemented"
    fun convF f [a,b,c] = tor(f(from1 a, from2 b, from3 c))
      | convF _ _ = raise Union.Never "arg mismatch"
    fun to f = toCfunction [ctype1, ctype2, ctype3] ctyper (convF f)
    in
    mkConversion from to (Cfunction ([ctype1, ctype2, ctype3], ctyper))
    end;

fun PASCALFUNCTION3 (c1, c2, c3) cr =
    let
    val (from1,_,ctype1) = breakConversion c1
    val (from2,_,ctype2) = breakConversion c2
    val (from3,_,ctype3) = breakConversion c3
    val (_,tor,ctyper) = breakConversion cr
    fun from _ = raise Foreign "Conversion from C function to ML function is not implemented"
    fun convF f [a,b,c] = tor(f(from1 a, from2 b, from3 c))
      | convF _ _ = raise Union.Never "arg mismatch"
    fun to f = toPascalfunction [ctype1, ctype2, ctype3] ctyper (convF f)
    in
    mkConversion from to (Cfunction ([ctype1, ctype2, ctype3], ctyper))
    end;
    
fun FUNCTION4 (c1, c2, c3, c4) cr =
    let
    val (from1,_,ctype1) = breakConversion c1
    val (from2,_,ctype2) = breakConversion c2
    val (from3,_,ctype3) = breakConversion c3
    val (from4,_,ctype4) = breakConversion c4
    val (_,tor,ctyper) = breakConversion cr
    fun from _ = raise Foreign "Conversion from C function to ML function is not implemented"
    fun convF f [a,b,c,d] = tor(f(from1 a, from2 b, from3 c, from4 d))
      | convF _ _ = raise Union.Never "arg mismatch"
    fun to f = toCfunction [ctype1, ctype2, ctype3, ctype4] ctyper (convF f)
    in
    mkConversion from to (Cfunction ([ctype1, ctype2, ctype3, ctype4], ctyper))
    end;

fun PASCALFUNCTION4 (c1, c2, c3, c4) cr =
    let
    val (from1,_,ctype1) = breakConversion c1
    val (from2,_,ctype2) = breakConversion c2
    val (from3,_,ctype3) = breakConversion c3
    val (from4,_,ctype4) = breakConversion c4
    val (_,tor,ctyper) = breakConversion cr
    fun from _ = raise Foreign "Conversion from C function to ML function is not implemented"
    fun convF f [a,b,c,d] = tor(f(from1 a, from2 b, from3 c, from4 d))
      | convF _ _ = raise Union.Never "arg mismatch"
    fun to f = toPascalfunction [ctype1, ctype2, ctype3, ctype4] ctyper (convF f)
    in
    mkConversion from to (Cfunction ([ctype1, ctype2, ctype3, ctype4], ctyper))
    end;
(*
val FUNCTION : 'a Conversion * 'b Conversion -> ( 'a -> 'b ) Conversion
val PASCALFUNCTION : 'a Conversion * 'b Conversion -> ( 'a -> 'b ) Conversion *)

end (* struct *)
    

