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

signature CInterfaceSig = sig

    
(**********************************************************************
 * ForeignException
 **********************************************************************)

    exception Foreign of string;
    
    
(**********************************************************************
 * ForeignDebug
 **********************************************************************)

    val get_foreign_debug  : unit -> int
    val set_foreign_debug  : int -> unit

    
(**********************************************************************
 * Ctype
 **********************************************************************)

    include CtypeSig
    
(**********************************************************************
 * LowerLevel
 **********************************************************************)

    type vol
    
    val alloc        : int -> Ctype -> vol
    val address      : vol -> vol
    val deref        : vol -> vol
    val offset       : int -> Ctype -> vol -> vol
    val assign       : Ctype -> vol -> vol -> unit
    
    val null         : vol

    val max_volatile_creation_info_depth : int ref;
    val inform_about_symbol_reload : bool ref;

    val fillCstring  : vol -> string -> unit
    
    val toCstring    : string -> vol
    val toCchar      : char -> vol
    val toCdouble    : real -> vol
    val toCfloat     : real -> vol
    val toCint       : int -> vol
    val toClong      : int -> vol
    val toCshort     : int -> vol
    val toCuint      : int -> vol
    val toCbytes     : Word8Vector.vector -> vol

    (* These take an argument type list, the result type and an ML function
       and create a C function value which can be passed to a C function.
       Functions with Pascal calling conventions are used for Windows callbacks. *)
    val toCfunction  : Ctype list -> Ctype -> (vol list -> vol) -> vol
    val toPascalfunction : Ctype list -> Ctype -> (vol list -> vol) -> vol
    (* We don't currently provide the reverse although we could probably use the
       call_sym function.
    val fromCfunction    : vol -> (Ctype * vol) list -> Ctype -> vol
    val fromPascalfunction : vol -> (Ctype * vol) list -> Ctype -> vol
    *)

    val fromCstring  : vol -> string
    val fromCchar    : vol -> char
    val fromCdouble  : vol -> real
    val fromCfloat   : vol -> real
    val fromCint     : vol -> int
    val fromClong    : vol -> int
    val fromCshort   : vol -> int
    val fromCuint    : vol -> int
    val fromCbytes   : vol * int -> Word8Vector.vector

    type dylib
    type sym
    
    val load_lib     : string -> dylib
    val load_sym     : dylib -> string -> sym
    val get_sym      : string -> string -> sym
    
    val setFinal     : sym -> vol -> unit

    val set_libPath  : dylib -> string -> unit
    val get_libPath  : dylib -> string

    val call_sym     : sym -> (Ctype * vol) list -> Ctype -> vol
    
    structure Union : UnionSig
    val call_sym_and_convert
        : sym -> (Ctype, vol) Union.directedArg list -> Ctype Union.unionChoice
        -> (vol Union.union * vol Union.union list)

    val volOfSym     : sym -> vol
    val mapSym       : (vol -> 'a) -> sym -> 'a

    
(**********************************************************************
 * CallWithConv
 **********************************************************************)

    type 'a Conversion;
    val mapConversion : ('a -> 'b) * ('b -> 'a) -> 'a Conversion -> 'b Conversion   
    val mkConversion : (vol -> 'a) -> ('a -> vol) -> Ctype -> 'a Conversion
    val breakConversion : 'a Conversion -> (vol -> 'a) * ('a -> vol) * Ctype

    val global  : sym -> 'a Conversion -> unit -> 'a
    val setGlobal:sym -> 'a Conversion -> 'a -> unit
    val call0   : sym -> unit -> 'a Conversion -> unit -> 'a
    val call1   : sym -> 'a Conversion -> 'b Conversion -> 'a -> 'b
    val call2   : sym -> 'a Conversion * 'b Conversion -> 'c Conversion -> 'a * 'b -> 'c
    
    val call3 : sym ->
      'a Conversion
    * 'b Conversion
    * 'c Conversion -> 'd Conversion
    -> 'a * 'b * 'c -> 'd
    
    val call4 : sym ->
      'a Conversion
    * 'b Conversion
    * 'c Conversion
    * 'd Conversion -> 'e Conversion
    -> 'a * 'b * 'c * 'd -> 'e

    val call5 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion -> 'f Conversion
    -> 'a * 'b * 'c * 'd * 'e -> 'f

    val call6 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion -> 'g Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f -> 'g

    val call7 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion 
    * 'g Conversion -> 'h Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f * 'g -> 'h

    val call8 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion 
    * 'g Conversion 
    * 'h Conversion -> 'i Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> 'i

    val call9 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion 
    * 'g Conversion 
    * 'h Conversion 
    * 'i Conversion -> 'j Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i -> 'j

    val call10 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion 
    * 'g Conversion 
    * 'h Conversion 
    * 'i Conversion 
    * 'j Conversion -> 'k Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j -> 'k

    val call11 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion 
    * 'g Conversion 
    * 'h Conversion 
    * 'i Conversion 
    * 'j Conversion
    * 'k Conversion -> 'l Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k -> 'l

    val call12 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion 
    * 'g Conversion 
    * 'h Conversion 
    * 'i Conversion 
    * 'j Conversion
    * 'k Conversion
    * 'l Conversion -> 'm Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l -> 'm

    val call13 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion 
    * 'g Conversion 
    * 'h Conversion 
    * 'i Conversion 
    * 'j Conversion
    * 'k Conversion
    * 'l Conversion
    * 'm Conversion -> 'n Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm -> 'n

    val call14 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion 
    * 'g Conversion 
    * 'h Conversion 
    * 'i Conversion 
    * 'j Conversion
    * 'k Conversion
    * 'l Conversion
    * 'm Conversion
    * 'n Conversion -> 'p Conversion 
    -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n -> 'p

    val call15 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion 
    * 'g Conversion 
    * 'h Conversion 
    * 'i Conversion 
    * 'j Conversion
    * 'k Conversion
    * 'l Conversion
    * 'm Conversion
    * 'n Conversion
    * 'p Conversion -> 'q Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'p -> 'q

    
    val call1ret1 : sym -> unit -> 'a Conversion -> unit -> 'a
    val call2ret1 : sym -> 'a Conversion -> 'b Conversion -> 'a -> 'b
    val call2ret2 : sym -> unit -> 'a Conversion * 'b Conversion -> unit -> 'a * 'b
    val call3ret1 : sym -> 'a Conversion * 'b Conversion -> 'c Conversion -> 'a * 'b -> 'c
    val call3ret2 : sym -> 'a Conversion -> 'b Conversion * 'c Conversion -> 'a -> 'b * 'c

    val call4ret1 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion -> 'd Conversion -> 'a * 'b * 'c -> 'd

    val call4ret2 : sym ->
      'a Conversion 
    * 'b Conversion -> 'c Conversion 
                     * 'd Conversion -> 'a * 'b -> 'c * 'd

    val call4ret3 : sym ->
      'a Conversion -> 'b Conversion
                     * 'c Conversion 
                     * 'd Conversion -> 'a -> 'b * 'c * 'd

    val call5ret1 : sym ->
         'a Conversion 
       * 'b Conversion 
       * 'c Conversion 
       * 'd Conversion -> 'e Conversion -> 'a * 'b * 'c * 'd -> 'e

    val call5ret2 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion -> 'd Conversion 
                     * 'e Conversion -> 'a * 'b * 'c -> 'd * 'e
       

(**********************************************************************
 * Struct
 **********************************************************************)

    val break_struct  : Ctype list -> vol -> vol list
    val make_struct   : (Ctype * vol) list -> vol

    
(**********************************************************************
 * Conversions
 **********************************************************************)

    val CHAR    : char Conversion
    val DOUBLE  : real Conversion
    val FLOAT   : real Conversion
    val INT     : int Conversion
    val LONG    : int Conversion
    val SHORT   : int Conversion
    val STRING  : string Conversion
    val VOID    : unit Conversion
    val BOOL    : bool Conversion
    val POINTER : vol Conversion
    val UINT    : int Conversion

    val POINTERTO : 'a Conversion -> 'a Conversion
    val STRING' : string Conversion

    
(**********************************************************************
 * StructConversionals
 **********************************************************************)

    val STRUCT2 : 'a Conversion * 'b Conversion -> ('a * 'b) Conversion
    val STRUCT3 : 'a Conversion * 'b Conversion * 'c Conversion -> ('a * 'b * 'c) Conversion
    val STRUCT4 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion -> ('a * 'b * 'c * 'd) Conversion
    val STRUCT5 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion * 'e Conversion -> ('a * 'b * 'c * 'd * 'e) Conversion
    val STRUCT6 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion * 'e Conversion * 'f Conversion -> ('a * 'b * 'c * 'd * 'e * 'f) Conversion
    val STRUCT7 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion * 'e Conversion * 'f Conversion * 'g Conversion -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) Conversion
    val STRUCT8 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion * 'e Conversion * 'f Conversion * 'g Conversion * 'h Conversion -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) Conversion
    val STRUCT9 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion * 'e Conversion * 'f Conversion * 'g Conversion * 'h Conversion * 'i Conversion -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) Conversion

(**********************************************************************
 * Function Conversionals
 **********************************************************************)
    
    val FUNCTION0 :     unit -> 'a Conversion -> ( unit -> 'a ) Conversion
    val PASCALFUNCTION0 : unit -> 'a Conversion -> ( unit -> 'a ) Conversion
    
    val FUNCTION1 : 'a Conversion -> 'b Conversion -> ( 'a -> 'b ) Conversion
    val PASCALFUNCTION1 : 'a Conversion -> 'b Conversion -> ( 'a -> 'b ) Conversion
    
    val FUNCTION2 : 'a Conversion * 'b Conversion -> 'c Conversion -> ( 'a * 'b -> 'c) Conversion
    val PASCALFUNCTION2 : 'a Conversion * 'b Conversion -> 'c Conversion -> ( 'a * 'b -> 'c) Conversion
    
    val FUNCTION3 : 'a Conversion * 'b Conversion * 'c Conversion -> 'd Conversion
                            -> ( 'a * 'b * 'c -> 'd) Conversion
    val PASCALFUNCTION3 : 'a Conversion * 'b Conversion * 'c Conversion -> 'd Conversion
                            -> ( 'a * 'b * 'c -> 'd) Conversion
    
    val FUNCTION4 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion -> 'e Conversion
                            -> ( 'a * 'b * 'c * 'd -> 'e) Conversion
    val PASCALFUNCTION4 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion -> 'e Conversion
                            -> ( 'a * 'b * 'c * 'd -> 'e) Conversion
end;
