(*
    Copyright (c) 2012 David C.J. Matthews

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
    type optVal
    type codeBinding

    type machineWord = Address.machineWord

    datatype argumentType =
        GeneralType
    |   FloatingPtType

    datatype varTuple =
        VarTupleSingle of { source: codetree, destOffset: codetree }
    |   VarTupleMultiple of
            { base: codetree, length: codetree, destOffset: codetree, sourceOffset: codetree }

    val mkClosLoad: int * bool -> codetree
    val mkGenLoad: int * int * bool * bool -> codetree
    
    val mkDecRef: codetree * int * int -> codeBinding
    val mkDec: int * codetree -> codeBinding
    val mkMutualDecs: (int * codetree) list -> codeBinding
    val mkNullDec: codetree -> codeBinding
    
    val mkTuple: codetree list -> codetree
    

    val mkIf: codetree * codetree * codetree -> codetree
    val mkEnv: codeBinding list * codetree -> codetree
    val mkConst: machineWord -> codetree
    val mkWhile: codetree * codetree -> codetree
    val mkCor: codetree * codetree -> codetree
    and mkCand: codetree * codetree -> codetree
    val mkRaise: codetree -> codetree
    and mkContainer: int -> codetree
    and mkIndirectVariable: {base: codetree, offset: codetree } -> codetree
    and mkTupleVariable: varTuple list * codetree -> codetree
    and mkAltMatch: codetree * codetree -> codetree
    and mkRecLoad: int -> codetree
    and mkEval: codetree * codetree list * bool -> codetree
    and mkHandle: codetree * codetree -> codetree
    and mkLoad: int * int -> codetree
    and mkInd: int * codetree -> codetree

    val mkLoop: codetree list -> codetree
    and mkBeginLoop: codetree * (int * codetree) list -> codetree
    
    val CodeFalse: codetree and CodeTrue: codetree and CodeZero: codetree
    
    val mkNot: codetree -> codetree
    and mkTestinteq: codetree * codetree -> codetree
    val mkTestnull:         codetree -> codetree
    val mkTestnotnull:      codetree -> codetree
    val mkTestptreq:        codetree * codetree -> codetree
    val mkStr: string -> codetree

    val mkTagTest:          codetree * word * word -> codetree
    val mkTupleSlice:       { base: codetree, offset: codetree, length: codetree } -> codetree
    val mkSetContainer:     codetree * codetree * int -> codetree
    val mkTupleFromContainer: codetree * int -> codetree

    val multipleUses: codetree * (unit -> int) * int -> {load: int -> codetree, dec: codeBinding list}

    val decSequenceWithFinalExp: codeBinding list -> codetree
    
    val sideEffectFree: codetree -> bool
    val makeConstVal: codetree -> codetree
    
    val evalue:    codetree -> machineWord option
    
    val findEntryInBlock: codetree -> int -> codetree

    val optGeneral: optVal -> codetree
    and optSpecial: optVal -> codetree
    and optDecs: optVal -> codeBinding list
    and optRec: optVal -> bool ref
    and optEnviron: optVal -> { addr : int,  level: int,  fpRel: bool, lastRef: bool } * int * int -> optVal
    and optVal:
        { general : codetree, special : codetree,
          environ : { addr : int,  level: int,  fpRel: bool, lastRef: bool } * int * int -> optVal,
          decs : codeBinding list, recCall: bool ref } -> optVal
    and simpleOptVal : codetree -> optVal
    
    val errorEnv: { addr : int,  level: int,  fpRel: bool, lastRef: bool } * int * int -> optVal
    
    val earlyRtsCall: machineWord -> bool
    
    val evaluate: codetree * (codetree * int -> unit -> machineWord) * int -> codetree

    structure Sharing:
    sig
        type codetree = codetree
        and  optVal = optVal
        and  argumentType = argumentType
        and  varTuple = varTuple
        and  codeBinding = codeBinding
    end

end;
