(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development copyright David C.J. Matthews 2011

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


functor CTYPE (structure Dispatch : DispatchSig) : CtypeSig  =
struct
    exception IllegalUseOfVoid

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

    (* generally useful definition *)
    val voidStar = Cpointer Cvoid

    local
        (* These are constants for a particular architecture so only
           need to be evaluated once. *)
        val alignCchar          = Dispatch.alignment Dispatch.Cchar
        and alignCdouble        = Dispatch.alignment Dispatch.Cdouble
        and alignCfloat         = Dispatch.alignment Dispatch.Cfloat
        and alignCint           = Dispatch.alignment Dispatch.Cint
        and alignClong          = Dispatch.alignment Dispatch.Clong
        and alignCshort         = Dispatch.alignment Dispatch.Cshort
        and alignCuint          = Dispatch.alignment Dispatch.Cuint
        and alignCpointer       = Dispatch.alignment Dispatch.Cpointer

        val sizeofCchar         = Dispatch.sizeof Dispatch.Cchar
        and sizeofCdouble       = Dispatch.sizeof Dispatch.Cdouble
        and sizeofCfloat        = Dispatch.sizeof Dispatch.Cfloat
        and sizeofCint          = Dispatch.sizeof Dispatch.Cint
        and sizeofClong         = Dispatch.sizeof Dispatch.Clong
        and sizeofCshort        = Dispatch.sizeof Dispatch.Cshort
        and sizeofCuint         = Dispatch.sizeof Dispatch.Cuint
        and sizeofCpointer      = Dispatch.sizeof Dispatch.Cpointer

    in
        fun alignment Cchar            = alignCchar
          | alignment Cdouble          = alignCdouble
          | alignment Cfloat           = alignCfloat
          | alignment Cint             = alignCint
          | alignment Clong            = alignClong
          | alignment Cshort           = alignCshort
          | alignment Cuint            = alignCuint
          | alignment (Cpointer _)     = alignCpointer
          | alignment (Cfunction _)    = alignCpointer
          | alignment (Cstruct ts)     = List.foldl(fn (t, n) => Int.max(alignment t, n)) 1 ts
          | alignment Cvoid            = raise IllegalUseOfVoid

        fun align n t =
        (******
         * returns first integer >= n
         * that satisfies alignment restrictions of the ctype t
         ******)
        let
            val a = alignment t
        in 
            a*((n-1) div a + 1)
        end

        fun sizeof Cchar            = sizeofCchar
          | sizeof Cdouble          = sizeofCdouble
          | sizeof Cfloat           = sizeofCfloat
          | sizeof Cint             = sizeofCint
          | sizeof Clong            = sizeofClong
          | sizeof Cshort           = sizeofCshort
          | sizeof (Cpointer _)     = sizeofCpointer
          | sizeof Cuint            = sizeofCuint
          | sizeof (Cfunction _)    = sizeofCpointer
          | sizeof (Cstruct ts)     =
                (* The size of a structure is the sum of the sizes of each element with
                   padding added before each so that it is correctly aligned.  Then the
                   structure is padded so that its size is a multiple of alignment. *)
                align (List.foldl(fn(t, n) => align n t + sizeof t) 0 ts) (Cstruct ts)
          | sizeof Cvoid            = raise IllegalUseOfVoid
 
    end

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
      | makeRaw (Cstruct l)       = Dispatch.Cstruct(map makeRaw l)
      | makeRaw Cvoid             = Dispatch.Cint (*hack*)

    
end; (* struct *)
