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
 This functor provides a wrapper around RawCtype, which reduces
 the number of calls to Underlying.sizeof & Underlying.alignment
 by saving size & alignment information into each Ctype value.
***) 

functor DISPATCH_SAVE_SIZEOF (Underlying : DispatchSig) : DispatchSig =
struct

    open Underlying
    
    abstype RawCtype = ctype of Underlying.RawCtype * int * int
    with
	fun wrap ct =
	    ctype (ct, Underlying.sizeof ct, Underlying.alignment ct)

        fun unpack      (ctype (ct,_,_))         = ct;
        fun sizeof      (ctype (_,size,_))       = size;
        fun alignment   (ctype (_,_,alignment))  = alignment;

    end;
    
    val Cchar           = wrap Underlying.Cchar;
    val Cdouble         = wrap Underlying.Cdouble;
    val Cfloat          = wrap Underlying.Cfloat;
    val Cint            = wrap Underlying.Cint;
    val Clong           = wrap Underlying.Clong;
    val Cpointer        = wrap Underlying.Cpointer
    val Cshort          = wrap Underlying.Cshort;
    val Cuint           = wrap Underlying.Cuint;
    fun Cstruct size    = wrap (Underlying.Cstruct size)

    fun call_sym_origCversion sym args ret =
	Underlying.call_sym_origCversion
	    sym
	    (map (fn (t,v) => (unpack t,v)) args)
	    (unpack ret)

    open Union;
    fun ID x = x;    
    fun call_sym_and_convert sym args ret =
	Underlying.call_sym_and_convert
	    sym
	    (map (mapDirectedArg unpack ID) args)
	    (mapUnionChoice unpack ret)

end;
