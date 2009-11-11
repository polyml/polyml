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
 *  Functor Definition
 **********************************************************************)

functor CTYPE
    (structure Dispatch : DispatchSig) : CtypeSig  =
struct    


exception IllegalUseOfVoid;

datatype Ctype =
    Cchar
  | Cdouble
  | Cfloat
  | Cint
  | Clong
  | Cshort
  | Cuint
  | Cpointer of Ctype
  | Cstruct of Ctype list
  | Cfunction of Ctype list * Ctype
  | Cvoid

(* generally usefull definition *)
val voidStar = Cpointer Cvoid;



fun max x y = if (x:int) >= y then x else y

fun maxlist [] acc = acc
  | maxlist (x::xs) acc = maxlist xs (max x acc)
    

fun alignment Cchar            = Dispatch.alignment Dispatch.Cchar
  | alignment Cdouble          = Dispatch.alignment Dispatch.Cdouble
  | alignment Cfloat           = Dispatch.alignment Dispatch.Cfloat
  | alignment Cint             = Dispatch.alignment Dispatch.Cint
  | alignment Clong            = Dispatch.alignment Dispatch.Clong
  | alignment Cshort           = Dispatch.alignment Dispatch.Cshort
  | alignment Cuint            = Dispatch.alignment Dispatch.Cuint
  | alignment (Cpointer _)     = Dispatch.alignment Dispatch.Cpointer
  | alignment (Cfunction _)    = Dispatch.alignment Dispatch.Cpointer
  | alignment (Cstruct ts)     = maxlist (map alignment ts) 1
  | alignment Cvoid            = raise IllegalUseOfVoid
    

fun align n t =
    (******
     * returns first integer >= n
     * that satisfies alignment restructions of the ctype t
     ******)
    let val a = alignment t
    in a*((n-1) div a+1)
    end

   
fun sizeof Cchar            = Dispatch.sizeof Dispatch.Cchar
  | sizeof Cdouble          = Dispatch.sizeof Dispatch.Cdouble
  | sizeof Cfloat           = Dispatch.sizeof Dispatch.Cfloat
  | sizeof Cint             = Dispatch.sizeof Dispatch.Cint
  | sizeof Clong            = Dispatch.sizeof Dispatch.Clong
  | sizeof Cshort           = Dispatch.sizeof Dispatch.Cshort
  | sizeof (Cpointer _)     = Dispatch.sizeof Dispatch.Cpointer
  | sizeof Cuint            = Dispatch.sizeof Dispatch.Cuint
  | sizeof (Cfunction _)    = Dispatch.sizeof Dispatch.Cpointer
  | sizeof (Cstruct ts)     = align (sizes 0 ts) (Cstruct ts)
  | sizeof Cvoid            = raise IllegalUseOfVoid
    

and sizes acc []            = acc
  | sizes acc (t::ts)       = sizes (align acc t + sizeof t) ts



type RawCtype = Dispatch.RawCtype;


fun makeRaw Cchar             = Dispatch.Cchar 
  | makeRaw Cdouble           = Dispatch.Cdouble
  | makeRaw Cfloat            = Dispatch.Cfloat
  | makeRaw Cint              = Dispatch.Cint
  | makeRaw Clong             = Dispatch.Clong
  | makeRaw Cshort            = Dispatch.Cshort
  | makeRaw Cuint             = Dispatch.Cuint
  | makeRaw (Cpointer _)      = Dispatch.Cpointer
  | makeRaw (Cfunction _)     = Dispatch.Cpointer (*Always the same as a pointer?*)
  | makeRaw (t as Cstruct _)  = Dispatch.Cstruct (sizeof t)
  | makeRaw Cvoid             = Dispatch.Cint (*hack*)

    
end; (* struct *)
