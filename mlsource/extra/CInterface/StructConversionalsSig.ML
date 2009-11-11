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


signature StructConversionalsSig =
sig
    type 'a Conversion

    val STRUCT2 : 'a Conversion * 'b Conversion -> ('a * 'b) Conversion
    val STRUCT3 : 'a Conversion * 'b Conversion * 'c Conversion -> ('a * 'b * 'c) Conversion
    val STRUCT4 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion -> ('a * 'b * 'c * 'd) Conversion
    val STRUCT5 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion * 'e Conversion -> ('a * 'b * 'c * 'd * 'e) Conversion
    val STRUCT6 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion * 'e Conversion * 'f Conversion -> ('a * 'b * 'c * 'd * 'e * 'f) Conversion
    val STRUCT7 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion * 'e Conversion * 'f Conversion * 'g Conversion -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) Conversion
    val STRUCT8 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion * 'e Conversion * 'f Conversion * 'g Conversion * 'h Conversion -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) Conversion
    val STRUCT9 : 'a Conversion * 'b Conversion * 'c Conversion * 'd Conversion * 'e Conversion * 'f Conversion * 'g Conversion * 'h Conversion * 'i Conversion -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) Conversion

    
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

end
