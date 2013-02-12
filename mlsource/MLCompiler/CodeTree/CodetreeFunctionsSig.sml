(*
    Copyright (c) 2012,13 David C.J. Matthews

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

signature CodetreeFunctionsSig =
sig
    type codetree
    and codeBinding

    type machineWord = Address.machineWord

    val mkLoadLocal: int -> codetree
    and mkLoadArgument: int -> codetree
    and mkLoadClosure: int -> codetree
 
    val mkEnv: codeBinding list * codetree -> codetree
    and mkInd: int * codetree -> codetree
    and mkTuple: codetree list -> codetree

    val CodeFalse: codetree and CodeTrue: codetree and CodeZero: codetree

    val mkSetContainer:     codetree * codetree * int -> codetree
    val mkTupleFromContainer: codetree * int -> codetree

    val decSequenceWithFinalExp: codeBinding list -> codetree
    
    val sideEffectFree: codetree -> bool
    val sideEffectFreeRTSCall: machineWord -> bool
    val makeConstVal: codetree -> codetree
    
    val evalue:    codetree -> machineWord option
    
    val findEntryInBlock: codetree -> int -> codetree
    
    val earlyRtsCall: machineWord -> bool

    structure Sharing:
    sig
        type codetree = codetree
        and codeBinding = codeBinding
    end

end;
