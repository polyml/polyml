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
 Defines datatypes used to communicate
 with new primitive: call_sym_and_convert.
***) 

signature UnionSig =
sig
  (* raised if a destructor is given the wrong argument *)
  exception Never of string;

(*
    datatype 'a union =
        Vol     of 'a
      | Char    of string
      | Double  of real
      | Float   of real
      | Int     of int
      | Long    of int
      | Short   of int
      | String  of string;
*)
    type 'a union
    
    val Vol    : 'a     -> 'a union;
    val Char   : char   -> 'a union;
    val Double : real   -> 'a union;
    val Float  : real   -> 'a union;
    val Int    : int    -> 'a union;
    val Long   : int    -> 'a union;
    val Short  : int    -> 'a union;
    val String : string -> 'a union;
    val Uint   : int    -> 'a union;

    val isVol    : 'a union -> bool;
    val isChar   : 'a union -> bool;
    val isDouble : 'a union -> bool;
    val isFloat  : 'a union -> bool;
    val isInt    : 'a union -> bool;
    val isLong   : 'a union -> bool;
    val isShort  : 'a union -> bool;
    val isString : 'a union -> bool;
    val isUint   : 'a union -> bool;

    val deVol    : 'a union -> 'a;
    val deChar   : 'a union -> char;
    val deDouble : 'a union -> real;
    val deFloat  : 'a union -> real;
    val deInt    : 'a union -> int;
    val deLong   : 'a union -> int;
    val deShort  : 'a union -> int;
    val deString : 'a union -> string;
    val deUint   : 'a union -> int;
    
(* ...
    datatype 'ctype unionChoice =
    chooseVol of 'ctype
      | chooseChar
      | chooseDouble
      | chooseFloat
      | chooseInt
      | chooseLong
      | chooseShort
      | chooseString;
... *)

    type 'a unionChoice;
  
    val chooseChar   : 'a unionChoice;
    val chooseDouble : 'a unionChoice;
    val chooseFloat  : 'a unionChoice;
    val chooseInt    : 'a unionChoice;
    val chooseLong   : 'a unionChoice;
    val chooseShort  : 'a unionChoice;
    val chooseString : 'a unionChoice;
    val chooseVol    : 'a -> 'a unionChoice;
    val chooseUint   : 'a unionChoice;

    val isChooseChar   : 'a unionChoice -> bool; 
    val isChooseDouble : 'a unionChoice -> bool; 
    val isChooseFloat  : 'a unionChoice -> bool; 
    val isChooseInt    : 'a unionChoice -> bool; 
    val isChooseLong   : 'a unionChoice -> bool; 
    val isChooseShort  : 'a unionChoice -> bool; 
    val isChooseString : 'a unionChoice -> bool; 
    val isChooseVol    : 'a unionChoice -> bool; 
    val isChooseUint   : 'a unionChoice -> bool; 

    val deChooseVol    : 'a unionChoice -> 'a;

(* ...
    datatype ('ctype, 'vol) directedArg =
    In of ('ctype * 'vol) union
      | Out of 'ctype unionChoice;
... *)

    type ('a,'b) directedArg;
    
    val In  : ('a * 'b) union -> ('a,'b) directedArg;
    val Out : 'a unionChoice  -> ('a,'b) directedArg;
     
    val isIn  : ('a,'b) directedArg -> bool;
    val isOut : ('a,'b) directedArg -> bool ;
     
    val deIn  : ('a,'b) directedArg -> ('a * 'b) union;
    val deOut : ('a,'b) directedArg -> 'a unionChoice;


    val mapUnion : ('a -> 'b) -> 'a union -> 'b union
    val mapUnionChoice : ('a -> 'b) -> 'a unionChoice -> 'b unionChoice
    val mapDirectedArg :
      ('a -> 'b) ->
      ('c -> 'd) -> ('a, 'c) directedArg -> ('b, 'd) directedArg

end;
