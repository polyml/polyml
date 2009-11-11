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

(* Clean up the name space by removing unnecessary declarations. *)

val _ = List.app PolyML.Compiler.forgetSignature
    ["ForeignExceptionSig", "CallWithConvSig", 
   "ConversionsSig", "ForeignDebugSig",
   "LowerLevelSig", "BehaviourRefsSig", 
   "StructSig", "DispatchSig", "VolatileSig", 
   "ForeignExceptionSig_Import", 
   "OrigLowerLevelSig", "StructConversionalsSig", "UnionSig",
   "CtypeSig"];

val _ = List.app PolyML.Compiler.forgetStructure
   [  "LowerLevel", "ForeignDebug", "VolBasic",
      "Struct", "Dispatch", "Volatile", 
      "Ctype", "StructConversionals",
      "CallWithConv", "BehaviourRefs", "ForeignException",
      "Conversions" ];

val _ = List.app PolyML.Compiler.forgetFunctor
   ["STRUCT", "CTYPE_SAVE_SIZEOF",
   "CINTERFACE", "CALL_WITH_CONV", "STRUCT_CONVERSIONALS",
   "CTYPE", "CONVERSIONS", 
   "VOLS_THAT_HOLD_REFS", "VOL_BASIC"];

