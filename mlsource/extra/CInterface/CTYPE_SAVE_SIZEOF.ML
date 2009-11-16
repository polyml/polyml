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

(***
 This functor provides a wrapper around Ctype, which reduces
 the number of calls to Underlying.sizeof & Underlying.alignment
 by saving size & alignment information into each Ctype value.
***) 

functor CTYPE_SAVE_SIZEOF (Underlying : CtypeSig) : CtypeSig =
struct

    open Underlying
    
    abstype Ctype = ctype of Underlying.Ctype * int * int
    with
    fun wrap ct =
        ctype (ct, Underlying.sizeof ct, Underlying.alignment ct)

    val Cvoid       = ctype (Underlying.Cvoid, 0, 0);
        
        fun unpack      (ctype (ct,_,_))         = ct;
        fun sizeof      (ctype (_,size,_))       = size;
        fun alignment   (ctype (_,_,alignment))  = alignment;
    end;
    

    val Cchar           = wrap Underlying.Cchar;
    val Cdouble         = wrap Underlying.Cdouble;
    val Cfloat          = wrap Underlying.Cfloat;
    val Cint            = wrap Underlying.Cint;
    val Clong           = wrap Underlying.Clong;
    val Cuint           = wrap Underlying.Cuint;
    val Cshort          = wrap Underlying.Cshort;

    val voidStar        = wrap Underlying.voidStar

    fun Cpointer t      = wrap (Underlying.Cpointer  (unpack t));
    fun Cfunction(ts,t) = wrap (Underlying.Cfunction (map unpack ts, unpack t));
    fun Cstruct ts      = wrap (Underlying.Cstruct   (map unpack ts));

    fun align n t       = Underlying.align n (unpack t);
    fun makeRaw t       = Underlying.makeRaw (unpack t);
                     

    local
        fun prettyCtype _ _ (_: Ctype) = PolyML.PrettyString "?"
    in
        val () = PolyML.addPrettyPrinter prettyCtype
    end
                
end;
