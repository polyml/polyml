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

signature LowerLevelSig = 
sig
    (* The first part of this is copied from VolatileSig *)
    type vol

    structure Ctype : CtypeSig
    structure BehaviourRefs : BehaviourRefsSig
    
    (* val load_lib     : string -> vol
    val load_sym     : vol -> string -> vol
        
    structure Union : UnionSig

    val call_sym_and_convert
        : vol -> (Ctype.Ctype, vol) Union.directedArg list -> Ctype.Ctype Union.unionChoice
        -> (vol Union.union * vol Union.union list) *)

    val alloc        : int -> Ctype.Ctype -> vol
    val address      : vol -> vol
    val deref        : vol -> vol
    val offset       : int -> Ctype.Ctype -> vol -> vol
    val assign       : Ctype.Ctype -> vol -> vol -> unit
        
    val toCchar      : char -> vol
    val toCdouble    : real -> vol
    val toCfloat     : real -> vol
    val toCint       : int -> vol
    val toClong      : int -> vol
    val toCshort     : int -> vol
    val toCuint      : int -> vol
        
    val fromCchar    : vol -> char
    val fromCdouble  : vol -> real
    val fromCfloat   : vol -> real
    val fromCint     : vol -> int
    val fromClong    : vol -> int
    val fromCshort   : vol -> int
    val fromCuint    : vol -> int

    val fillCstring  : vol -> string -> unit
    val toCstring    : string -> vol
    val fromCstring  : vol -> string

    val toCbytes     : Word8Vector.vector -> vol
    val fromCbytes   : vol * int -> Word8Vector.vector
    
    val toCfunction  : Ctype.Ctype list -> Ctype.Ctype -> (vol list -> vol) -> vol
    val toPascalfunction : Ctype.Ctype list -> Ctype.Ctype -> (vol list -> vol) -> vol

    type dylib
    type sym

    (* overwite specifications in VolatileSig *)
    val load_lib     : string -> dylib
    val load_sym     : dylib -> string -> sym
    val get_sym  : string -> string -> sym
    
    val setFinal     : sym -> vol -> unit

    val set_libPath  : dylib -> string -> unit
    val get_libPath  : dylib -> string

    structure Union : UnionSig

    val call_sym_and_convert
        : sym -> (Ctype.Ctype, vol) Union.directedArg list -> Ctype.Ctype Union.unionChoice
        -> (vol Union.union * vol Union.union list)

    val volOfSym     : sym -> vol
    val mapSym       : (vol -> 'a) -> sym -> 'a
        
    (*end*)
end;
