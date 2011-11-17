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


signature DispatchSig =
sig
    type rawvol
 
    type RawCtype
    
    val Cchar     : RawCtype
    val Cdouble   : RawCtype
    val Cfloat    : RawCtype
    val Cint      : RawCtype
    val Clong     : RawCtype
    val Cpointer  : RawCtype
    val Cshort    : RawCtype
    val Cstruct   : RawCtype list -> RawCtype
    val Cuint     : RawCtype
    
    val get_foreign_debug  : unit -> int
    val set_foreign_debug  : int -> unit
        
    val load_lib     : string -> rawvol
    val load_sym     : rawvol -> string -> rawvol

    structure Union : UnionSig
    
    val call_sym_and_convert
        : rawvol -> (RawCtype, rawvol) Union.directedArg list -> RawCtype Union.unionChoice
        -> (rawvol Union.union * rawvol Union.union list)
    
    val alloc        : int -> rawvol
    val address      : rawvol -> rawvol
    val deref        : rawvol -> rawvol
    val offset       : rawvol -> int -> rawvol
    val assign       : rawvol -> rawvol -> int -> unit
    val sizeof       : RawCtype -> int
    val alignment    : RawCtype -> int

    val toCchar      : char -> rawvol
    val toCdouble    : real -> rawvol
    val toCfloat     : real -> rawvol
    val toCint       : int -> rawvol
    val toClong      : int -> rawvol
    val toCshort     : int -> rawvol
    val toCuint      : int -> rawvol

    val fromCchar    : rawvol -> char
    val fromCdouble  : rawvol -> real
    val fromCfloat   : rawvol -> real
    val fromCint     : rawvol -> int
    val fromClong    : rawvol -> int
    val fromCshort   : rawvol -> int
    val fromCuint    : rawvol -> int

    val fillCstring  : rawvol -> string -> unit
    val toCstring    : string -> rawvol
    val fromCstring  : rawvol -> string

    val toCbytes     : Word8Vector.vector -> rawvol
    val fromCbytes   : rawvol * int -> Word8Vector.vector
    
    val toCfunction  : RawCtype list -> RawCtype -> (rawvol list -> rawvol) -> rawvol
    val toPascalfunction : RawCtype list -> RawCtype -> (rawvol list -> rawvol) -> rawvol
    
    val setFinal     : rawvol -> rawvol -> unit
    
    val null         : rawvol

end;
