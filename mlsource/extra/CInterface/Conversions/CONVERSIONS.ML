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
 *  Import Signatures
 **********************************************************************)

signature ForeignExceptionSig_Import =
sig
    val Foreign : string -> exn
end;

(**********************************************************************
 *  Functor Definition
 **********************************************************************)

functor CONVERSIONS (
    structure LowerLevel : LowerLevelSig
    structure CallWithConv : CallWithConvSig
    structure ForeignException : ForeignExceptionSig_Import

    sharing type LowerLevel.vol = CallWithConv.vol
    sharing LowerLevel.Ctype = CallWithConv.Ctype

) : ConversionsSig =
struct    

    
open LowerLevel;
open LowerLevel.Ctype;    
open CallWithConv;
open ForeignException;
    

(* Generally we don't want to convert a unit value into a void except in the
   case when we're returning a void result from a callback.  Just return
   zero in that case.  *)
val VOID = mkConversion (fn _ => ()) (fn () => toCint 0) Cvoid

local    
    fun int2bool 0      = false
      | int2bool _      = true
    
    fun bool2int false  = 0
      | bool2int true   = 1
in
    val BOOL = mapConversion (int2bool,bool2int) INT;
end


fun ID x = x;
val POINTER = mkConversion ID ID (Cpointer Cvoid)


fun POINTERTO conv =
    let val (from,to,ctype) = breakConversion conv
    in mkConversion (from o deref) (address o to) (Cpointer ctype)
    end;


(***
 The conversion STRING' is Like STRING conversion,
 except when converting from C to ML, it also copes with NULL pointers.
 (However, the ML string created is identical to the ML string created
 when converting a pointer to a null.)
***) 

val STRING' = mkConversion
                    (fn v => if fromCint v = 0 then "" else fromCstring v)
            toCstring
            (Cpointer Cchar);


    
end (* struct *)
    
