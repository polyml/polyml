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
    and       Arm64Assembly: Arm64Assembly
    and       Debug: DEBUG
    
    sharing FallBackCG.Sharing = BackendTree.Sharing = CodeArray.Sharing = Arm64Assembly.Sharing
) : GENCODESIG =
struct

    open BackendTree CodeArray Arm64Assembly Address
    
    (* tag a short constant *)
    fun tag c = 2 * c + 1
  
    (* shift a short constant, but don't set tag bit *)
    fun semitag c = 2 * c

    
    fun codegen (pt, cvec, resultClosure, numOfArgs, localCount, parameters) =
    let
        val maxStack = ref 0
        val () = genPushReg (X0, cvec)
        val () = genPushReg (X30, cvec)
               
        fun genCode (BICConstnt(w, _)) =
            if isShort w andalso toShort w < 0w32768 (* So tagged value will fit. *)
            then
            let
                val cVal = tag(Word.toInt(toShort w))
            in
                genMoveShortConstToReg(X0, cVal, cvec); genPushReg(X0, cvec)
            end
            else raise Fallback
            
        |   genCode _ = raise Fallback
        
        val () = genCode pt

        val () = genPopReg(X0, cvec)
        val () = genPopReg(X30, cvec)
        val () = genIncMLSP1 cvec
        val () = genRetCode cvec
    in
        generateCode{code = cvec, maxStack = !maxStack, resultClosure=resultClosure}
    end
    
    
    fun gencodeLambda(lambda as { name, body, argTypes, localCount, ...}:bicLambdaForm, parameters, closure) =
    (let
        val debugSwitchLevel = Debug.getParameter Debug.compilerDebugTag parameters
        val _ = debugSwitchLevel <> 0 orelse raise Fallback
    
        (* make the code buffer for the new function. *)
        val newCode : code = codeCreate (name, parameters)
        (* This function must have no non-local references. *)
        
        val _ =
            case (argTypes, localCount) of
                ([GeneralType], 0) => ()
            |   _ => raise Fallback
    in
        codegen (body, newCode, closure, List.length argTypes, localCount, parameters)
    end) handle Fallback => FallBackCG.gencodeLambda(lambda, parameters, closure)

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
