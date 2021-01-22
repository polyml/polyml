(*
    Copyright (c) 2021 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.
    
    You should have received a copy of the GNU Lesser General Public
    Licence along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor Arm64GenCode (
    structure FallBackCG: GENCODESIG
    and       BackendTree: BackendIntermediateCodeSig
    and       CodeArray: CODEARRAYSIG
    
    sharing FallBackCG.Sharing = BackendTree.Sharing = CodeArray.Sharing
) : GENCODESIG =
struct

    open BackendTree CodeArray
    
    exception Fallback
    
    fun gencodeLambda(lambda, args, closureRef) =
    (
        raise Fallback
    )
    (* If we can't do it fall back to the interpreter. *)
    handle Fallback => FallBackCG.gencodeLambda(lambda, args, closureRef)
    
    
    
    structure Foreign: FOREIGNCALLSIG =
    struct
        open FallBackCG.Foreign
    end

    structure Sharing =
    struct
        type backendIC = backendIC
        and argumentType = argumentType
        and bicLoadForm = bicLoadForm
        and closureRef = closureRef
    end
end;
