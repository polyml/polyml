(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited
        
    Modified David C. J. Matthews 2009.

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

(*****************************************************************************)
(*                  STRUCTURES export signature                              *)
(*****************************************************************************)
signature STRUCTURESSIG =
sig
    type structs;
    type structVals;
    type types;
    type parsetree;
    type lexan;
    type pretty;
    type values;
    type typeConstrs;
    type codetree;
    type signatures;
    type functors;
    type env;
    type sigBind and functorBind and structBind
    type machineWord
    type fixStatus
    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
    type topdec
    type program
    type ptProperties
    type exportTree = location * ptProperties list
    type typeParsetree
    type formalArgStruct

    val isEmptyStruct:      structs -> bool;
    val emptyStruct:        structs
    val mkStructureDec:     structBind list * location -> structs;
    val mkStruct:           structs list * location -> structs;
    val mkSignatureDec:     sigBind list * location -> topdec;
    val mkSig:              structs list * location -> structs;
    val mkFunctorDec:       functorBind list * location -> topdec;
    val mkInclude:          structs list -> structs;
    val mkLocaldec:         structs list * structs list * bool * location -> structs;
    val mkCoreLang:         parsetree * location -> structs;
    val mkStructureBinding: (string * location) * (structs * bool * location) option * structs option * location -> structBind
    val mkStructIdent:      string * location -> structs;
    val mkSigIdent:         string * location -> structs;
    val mkSignatureBinding: (string * location) * structs * location -> sigBind;
    val mkValSig:           (string * location) * typeParsetree * location -> structs;
    val mkExSig:            (string * location) * typeParsetree option * location -> structs;
    val mkFunctorAppl:      string * structs * location * location -> structs;
    val mkFormalArg:        string * structs -> formalArgStruct;
    val mkFunctorBinding:
        string * location * (structs * bool * location) option * structs * formalArgStruct * location -> functorBind;
    val mkSharing:          bool * string list * location -> structs;
    val mkWhereType:          structs * types list * string * types * location -> structs
    val mkSigConstraint:    structs * structs * bool * location -> structs
    val mkTopDec:           structs -> topdec
    val mkProgram:          topdec list * location -> program

    val pass2Structs:   program * lexan * env -> unit;

    val pass4Structs:
        codetree * program ->
            {
                fixes: (string * fixStatus) list, values: (string * values) list,
                structures: (string * structVals) list, signatures: (string * signatures) list,
                functors: (string * functors) list, types: (string* typeConstrs) list
            };

    val gencodeStructs: program * lexan -> codetree;

    val displayProgram: program * int -> pretty;

    val structsExportTree: (unit->exportTree) option * program -> exportTree

    structure Sharing:
    sig
        type structs        = structs
        type structVals     = structVals
        type types          = types
        type parsetree      = parsetree
        type lexan          = lexan
        type pretty         = pretty
        type values         = values
        type typeConstrs    = typeConstrs
        type codetree       = codetree
        type signatures     = signatures
        type functors       = functors
        type env            = env
        type sigBind        = sigBind
        and  functorBind    = functorBind
        and  structBind     = structBind
        type machineWord    = machineWord
        type fixStatus      = fixStatus
        type topdec         = topdec
        type program        = program
        type typeParsetree  = typeParsetree
        type formalArgStruct= formalArgStruct
        type ptProperties   = ptProperties
    end
end;
