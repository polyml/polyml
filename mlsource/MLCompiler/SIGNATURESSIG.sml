(*
    Copyright David C. J. Matthews 2009

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

signature SIGNATURESSIG =
sig
    type sigs
    type structSigBind
    type types
    type parsetree
    type typeParsetree
    type typeVarForm
    type pretty
    type ptProperties
    type env
    type signatures
    type lexan
    type typeId
    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
    type exportTree = location * ptProperties list
    type navigation =
        {parent: (unit -> exportTree) option,
         next: (unit -> exportTree) option,
         previous: (unit -> exportTree) option}

    val mkStructureSig:     structSigBind list * location -> sigs
    val mkStructureSigBinding: (string * location) * (sigs * bool * location) * location -> structSigBind
    val mkValSig:           (string * location) * typeParsetree * location -> sigs
    val mkExSig:            (string * location) * typeParsetree option * location -> sigs
    val mkCoreType:         parsetree * location -> sigs
    val mkSharing:          bool * string list * location -> sigs
    val mkWhereType:        sigs * typeVarForm list * string * types * location -> sigs
    val mkInclude:          sigs list * location -> sigs
    val mkSigIdent:         string * location -> sigs;
    val mkSig:              sigs list * location -> sigs;
    val emptySig:           sigs

    val displaySigs: sigs * int -> pretty
    val sigExportTree: navigation * sigs -> exportTree 
    val sigVal: sigs * int * (int->typeId) * env * lexan * location -> signatures
    val functorArgSigval: sigs * int * (int->typeId) * env * lexan * location -> signatures
    val undefinedSignature: signatures

    structure Sharing:
    sig
        type sigs           = sigs
        type structSigBind  = structSigBind
        type signatures     = signatures
        type types          = types
        type parsetree      = parsetree
        type typeParsetree  = typeParsetree
        type typeVarForm    = typeVarForm
        type pretty         = pretty
        type ptProperties   = ptProperties
        type env            = env
        type lexan          = lexan
        type typeId         = typeId
    end
end;
