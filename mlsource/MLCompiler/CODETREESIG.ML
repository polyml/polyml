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

signature CODETREESIG =
sig
    type machineWord
    type codetree
    type pretty

    datatype argumentType =
        GeneralType
    |   FloatingPtType

    datatype varTuple =
        VarTupleSingle of { source: codetree, destOffset: codetree }
    |   VarTupleMultiple of
            { base: codetree, length: codetree, destOffset: codetree, sourceOffset: codetree }
 
    val isCodeNil:          codetree -> bool;
    val CodeNil:            codetree; (* Empty codetree NOT the code for "nil" *)
    val CodeTrue:           codetree; (* code for "true"  *)
    val CodeFalse:          codetree; (* code for "false" *)
    val CodeZero:           codetree; (* code for 0, nil etc. *)

    val mkFunction:         codetree * argumentType list * argumentType * string -> codetree
    val mkInlineFunction:   codetree * argumentType list * argumentType * string -> codetree
    val mkCall:             codetree * (codetree * argumentType) list * argumentType -> codetree

    val MatchFail:          codetree; (* pattern match has failed *)
    val mkAltMatch:         codetree * codetree -> codetree;

    val mkRecLoad:          int-> codetree;
    val mkLoad:             int * int -> codetree;
    val mkConst:            machineWord -> codetree;
    val mkDec:              int * codetree -> codetree;
    val mkInd:              int * codetree -> codetree;
    val mkProc:             codetree * int * string -> codetree;
    val mkInlproc:          codetree * int * string -> codetree;
    val mkMacroProc:        codetree * int * string -> codetree;
    val mkIf:               codetree * codetree * codetree -> codetree;
    val mkWhile:            codetree * codetree -> codetree;
    val mkEnv:              codetree list -> codetree;
    val mkStr:              string -> codetree;
    val mkTuple:            codetree list -> codetree;
    val mkMutualDecs:       codetree list -> codetree;
    val mkRaise:            codetree -> codetree;
    val mkNot:              codetree -> codetree;
    val mkTestnull:         codetree -> codetree;
    val mkTestnotnull:      codetree -> codetree;
    val mkCor:              codetree * codetree -> codetree;
    val mkCand:             codetree * codetree -> codetree;
    val mkTestptreq:        codetree * codetree -> codetree;
    val mkTestinteq:        codetree * codetree -> codetree;
    val mkHandle:           codetree * codetree -> codetree
    val mkEval:             codetree * codetree list * bool -> codetree;
    val identityFunction:   string -> codetree;
    val Ldexc:              codetree;
    val mkContainer:        int -> codetree
    val mkSetContainer:     codetree * codetree * int -> codetree
    val mkTupleFromContainer: codetree * int -> codetree
    val mkTagTest:          codetree * word * word -> codetree
    val mkIndirectVariable: { base: codetree, offset: codetree } -> codetree
    val mkTupleSlice:       { base: codetree, offset: codetree, length: codetree } -> codetree
    val mkTupleVariable:    varTuple list * codetree -> codetree

    val multipleUses: codetree * (unit -> int) * int -> {load: int -> codetree, dec: codetree list}

    val pretty:    codetree -> pretty;
    val evalue:    codetree -> machineWord option
    val genCode:   codetree * Universal.universal list -> (unit -> codetree);
    
    val rtsFunction:        int -> codetree

    structure Sharing:
    sig
        type machineWord = machineWord
        type codetree    = codetree
        type pretty      = pretty
        type argumentType=argumentType
    end

end;

