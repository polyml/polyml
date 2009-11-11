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

signature CallWithConvSig =
sig
    type sym
    type vol
    
    structure Ctype : CtypeSig

    type 'a Conversion;

    val CHAR    : char Conversion
    val FLOAT   : real Conversion
    val DOUBLE  : real Conversion
    val INT     : int Conversion
    val SHORT   : int Conversion
    val LONG    : int Conversion
    val STRING  : string Conversion
    val UINT    : int Conversion
    
    val mapConversion : ('a -> 'b) * ('b -> 'a) -> 'a Conversion -> 'b Conversion
    val mkConversion : (vol -> 'a) -> ('a -> vol) -> Ctype.Ctype -> 'a Conversion
    val breakConversion : 'a Conversion -> (vol -> 'a) * ('a -> vol) * Ctype.Ctype

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

end;
