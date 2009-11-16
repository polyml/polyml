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
 *  Functor Definition.
 **********************************************************************)
 
functor CALL_WITH_CONV
    (structure LowerLevel : LowerLevelSig)
         : CallWithConvSig =
struct

structure Ctype = LowerLevel.Ctype    

open LowerLevel
open Ctype

(* ...
exception Never of string;
fun never s = raise Never s;
... *)

fun never s = raise Union.Never s;
    
local open Union
in

(* now defined in Union ...
val deVol    = fn (Vol x) =>    x | _ => never "deVol";
val deChar   = fn (Char x) =>   x | _ => never "deChar";
val deFloat  = fn (Float x) =>  x | _ => never "deFloat";
val deDouble = fn (Double x) => x | _ => never "deDouble";
val deInt    = fn (Int x) =>    x | _ => never "deInt";
val deShort  = fn (Short x) =>  x | _ => never "deShort";
val deLong   = fn (Long x) =>   x | _ => never "deLong";
val deString = fn (String x) => x | _ => never "deString";
... *)


abstype 'a Conversion=
    Conversion of (vol union -> 'a)
                * ('a -> (Ctype * vol) union)
                * Ctype unionChoice
with

infix @
infix @@

fun x @ (Conversion(_,to,_))            = In (to x)
fun (Conversion(from,_,choice)) @@ pap  =
    case (pap choice) of
        (union,[]) => from union
      | _ => never "@@"

val ignore = chooseVol Cvoid
fun want (Conversion(_,_,choice)) = Out choice;
fun from (Conversion(fr,_,_)) x = fr x;


(**********************************************************************
 * Special Conversion (efficient)
 **********************************************************************)
    
val CHAR   = Conversion (deChar,     Char,     chooseChar);
val FLOAT  = Conversion (deFloat,    Float,    chooseFloat);
val DOUBLE = Conversion (deDouble,   Double,   chooseDouble);
val INT    = Conversion (deInt,      Int,      chooseInt);
val SHORT  = Conversion (deShort,    Short,    chooseShort);
val LONG   = Conversion (deLong,     Long,     chooseLong);
val STRING = Conversion (deString,   String,   chooseString);
val UINT   = Conversion (deUint,     Uint,     chooseUint); (* Added DCJM 17/5/01. *)

    
fun mapConversion  (from',to') (Conversion(from,to,choice)) =
    Conversion
        (fn x => from' (from x),
         fn x => to (to' x),
         choice)

fun mkConversion from to ctype =
    Conversion
        (fn v => if isVol v then from (deVol v) else never "mkConversion",
         fn x => Vol (ctype, to x),
         chooseVol ctype)
        

val Cstring = Cpointer Cchar;
fun ID x = x;    

(* ...
fun breakConversion (Conversion(from,to,choice)) =
let
  fun makeBreaker (fromSpecial,toSpecial,ctype,fromUnion,toUnion) =
     (fn vol => from (toUnion (fromSpecial vol)),
      fn x =>  toSpecial (fromUnion (to x)),
      ctype)
in
  case choice of
    chooseVol ctype => makeBreaker (ID, ID, ctype, fn x => #2 (deVol x), Vol)
  | chooseChar      => makeBreaker (fromCchar,   toCchar,   Cchar,   deChar,   Char)
  | chooseFloat     => makeBreaker (fromCfloat,  toCfloat,  Cfloat,  deFloat,  Float)
  | chooseDouble    => makeBreaker (fromCdouble, toCdouble, Cdouble, deDouble, Double)
  | chooseInt       => makeBreaker (fromCint,    toCint,    Cint,    deInt,    Int)
  | chooseShort     => makeBreaker (fromCshort,  toCshort,  Cshort,  deShort,  Short)
  | chooseLong      => makeBreaker (fromClong,   toClong,   Clong,   deLong,   Long)
  | chooseString    => makeBreaker (fromCstring, toCstring, Cstring, deString, String)
end
... *)

fun breakConversion (Conversion(from,to,choice)) =
let
  fun makeBreaker (fromSpecial,toSpecial,ctype,fromUnion,toUnion) =
     (fn vol => from (toUnion (fromSpecial vol)),
      fn x =>  toSpecial (fromUnion (to x)),
      ctype)
in
  if isChooseVol    choice then makeBreaker (ID, ID, deChooseVol choice, fn x => #2 (deVol x), Vol) else
  if isChooseChar   choice then makeBreaker (fromCchar,   toCchar,   Cchar,   deChar,   Char)   else
  if isChooseFloat  choice then makeBreaker (fromCfloat,  toCfloat,  Cfloat,  deFloat,  Float)  else
  if isChooseDouble choice then makeBreaker (fromCdouble, toCdouble, Cdouble, deDouble, Double) else
  if isChooseInt    choice then makeBreaker (fromCint,    toCint,    Cint,    deInt,    Int)    else
  if isChooseShort  choice then makeBreaker (fromCshort,  toCshort,  Cshort,  deShort,  Short)  else
  if isChooseLong   choice then makeBreaker (fromClong,   toClong,   Clong,   deLong,   Long)   else
  if isChooseUint   choice then makeBreaker (fromCuint,   toCuint,   Cuint,   deUint,   Uint)   else
  (* ishooseString choice *)    makeBreaker (fromCstring, toCstring, Cstring, deString, String)
end

end (* abstype *)
end (* local open Union *)



fun global sym conv () =
    (******
     * Access to global constants.
     * Reads the value each time final unit is applied.
     ******)
    let val (from,_,_) = breakConversion conv
    in from (mapSym deref sym)
    end;
        

fun setGlobal sym conv v =
    (******
     * Set the value of a global constant to a new value.
     ******)
    let val (_,to,ctype) = breakConversion conv
    in assign ctype (mapSym deref sym) (to v)
    end;

    
local
    fun call s = call_sym_and_convert s
in
    fun call0  sym ()                              RET ()                              = RET @@ call sym []
    fun call1  sym (A)                             RET (a)                             = RET @@ call sym [a@A]
    fun call2  sym (A,B)                           RET (a,b)                           = RET @@ call sym [a@A,b@B]
    fun call3  sym (A,B,C)                         RET (a,b,c)                         = RET @@ call sym [a@A,b@B,c@C]
    fun call4  sym (A,B,C,D)                       RET (a,b,c,d)                       = RET @@ call sym [a@A,b@B,c@C,d@D]
    fun call5  sym (A,B,C,D,E)                     RET (a,b,c,d,e)                     = RET @@ call sym [a@A,b@B,c@C,d@D,e@E]
    fun call6  sym (A,B,C,D,E,F)                   RET (a,b,c,d,e,f)                   = RET @@ call sym [a@A,b@B,c@C,d@D,e@E,f@F]
    fun call7  sym (A,B,C,D,E,F,G)                 RET (a,b,c,d,e,f,g)                 = RET @@ call sym [a@A,b@B,c@C,d@D,e@E,f@F,g@G]
    fun call8  sym (A,B,C,D,E,F,G,H)               RET (a,b,c,d,e,f,g,h)               = RET @@ call sym [a@A,b@B,c@C,d@D,e@E,f@F,g@G,h@H]
    fun call9  sym (A,B,C,D,E,F,G,H,I)             RET (a,b,c,d,e,f,g,h,i)             = RET @@ call sym [a@A,b@B,c@C,d@D,e@E,f@F,g@G,h@H,i@I]
    fun call10 sym (A,B,C,D,E,F,G,H,I,J)           RET (a,b,c,d,e,f,g,h,i,j)           = RET @@ call sym [a@A,b@B,c@C,d@D,e@E,f@F,g@G,h@H,i@I,j@J]
    fun call11 sym (A,B,C,D,E,F,G,H,I,J,K)         RET (a,b,c,d,e,f,g,h,i,j,k)         = RET @@ call sym [a@A,b@B,c@C,d@D,e@E,f@F,g@G,h@H,i@I,j@J,k@K]
    fun call12 sym (A,B,C,D,E,F,G,H,I,J,K,L)       RET (a,b,c,d,e,f,g,h,i,j,k,l)       = RET @@ call sym [a@A,b@B,c@C,d@D,e@E,f@F,g@G,h@H,i@I,j@J,k@K,l@L]
    fun call13 sym (A,B,C,D,E,F,G,H,I,J,K,L,M)     RET (a,b,c,d,e,f,g,h,i,j,k,l,m)     = RET @@ call sym [a@A,b@B,c@C,d@D,e@E,f@F,g@G,h@H,i@I,j@J,k@K,l@L,m@M]
    fun call14 sym (A,B,C,D,E,F,G,H,I,J,K,L,M,N)   RET (a,b,c,d,e,f,g,h,i,j,k,l,m,n)   = RET @@ call sym [a@A,b@B,c@C,d@D,e@E,f@F,g@G,h@H,i@I,j@J,k@K,l@L,m@M,n@N]
    fun call15 sym (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) RET (a,b,c,d,e,f,g,h,i,j,k,l,m,n,p) = RET @@ call sym [a@A,b@B,c@C,d@D,e@E,f@F,g@G,h@H,i@I,j@J,k@K,l@L,m@M,n@N,p@O]

        
    fun call1ret1 sym () (X) () =
        case call sym [ want X ] ignore of
            (_,[x]) => (from X x)
          | _ => never "call1ret1"

    fun call2ret1 sym (A) (X) (a) =
        case call sym [ a@A, want X ] ignore of
            (_,[x]) => (from X x)
          | _ => never "call2ret1"

    fun call2ret2 sym () (X,Y) () =
        case call sym [ want X, want Y ] ignore of
            (_,[x,y]) => (from X x, from Y y)
          | _ => never "call2ret2"

    fun call3ret1 sym (A,B) (X) (a,b) =
        case call sym [ a@A, b@B, want X ] ignore of
            (_,[x]) => (from X x)
          | _ => never "call3ret1"

    fun call3ret2 sym (A) (X,Y) (a) =
        case call sym [ a@A, want X, want Y ] ignore of
            (_,[x,y]) => (from X x, from Y y)
          | _ => never "call3ret2"

    fun call4ret1 sym (A,B,C) (X) (a,b,c) =
        case call sym [ a@A, b@B, c@C, want X ] ignore of
            (_,[x]) => (from X x)
          | _ => never "call4ret1"

    fun call4ret2 sym (A,B) (X,Y) (a,b) =
        case call sym [ a@A, b@B, want X, want Y ] ignore of
            (_,[x,y]) => (from X x, from Y y)
          | _ => never "call4ret2"

    fun call4ret3 sym (A) (X,Y,Z) (a) =
        case call sym [ a@A, want X, want Y, want Z ] ignore of
            (_,[x,y,z]) => (from X x, from Y y, from Z z)
          | _ => never "call4ret3"

    fun call5ret1 sym (A,B,C,D) (X) (a,b,c,d) =
        case call sym [ a@A, b@B, c@C, d@D, want X ] ignore of
            (_,[x]) => (from X x)
          | _ => never "call5ret1"

    fun call5ret2 sym (A,B,C) (X,Y) (a,b,c) =
        case call sym [ a@A, b@B, c@C, want X, want Y ] ignore of
            (_,[x,y]) => (from X x, from Y y)
          | _ => never "call5ret2"

        
end (* local *)



    local
        fun prettyConv _ _ (_: 'a Conversion) = PolyML.PrettyString "?"
    in
        val () = PolyML.addPrettyPrinter prettyConv
    end

end (* struct *)
