(*
    Copyright (c) 2016 David C.J. Matthews

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

signature CodegenTreeSig =
sig
    type codetree and inlineStatus and loadForm and argumentType and codeUse
    type lambdaForm =
    {
        body          : codetree,
        isInline      : inlineStatus,
        name          : string,
        closure       : loadForm list,
        argTypes      : (argumentType * codeUse list) list,
        resultType    : argumentType,
        localCount    : int,
        recUse        : codeUse list
    }
    type machineWord = Address.machineWord
    val codeGenerate: lambdaForm * Universal.universal list * Address.address -> Universal.universal list
    structure Sharing :
    sig
        type codetree = codetree
        and inlineStatus = inlineStatus
        and loadForm = loadForm
        and argumentType = argumentType
        and codeUse = codeUse
    end
end;
